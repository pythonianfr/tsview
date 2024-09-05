port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Dateinterval exposing (medianValue)
import Dict exposing (Dict)
import Set exposing (Set)
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , initHorizon
    , horizons
    , getFromToDates
    , loadFromLocalStorage
    , updateHorizon
    , updateHorizonFromData
    , extractZoomDates
    , setStatusPlot
    )
import Horizon as ModuleHorizon
import Http
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Info as I
import Json.Decode as JD
import Maybe.Extra as Maybe
import Metadata as M
import OrderedDict as OD
import Plotter exposing
    ( defaultLayoutOptions
    , defaultoptions
    , getdata
    )
import Process as P
import Url.Builder as UB
import Util as U
import Json.Encode as JE
import Task as T

port copyToClipboard : String -> Cmd msg
port dateInInterval : (List String -> msg) -> Sub msg
port panActive : (Bool -> msg) -> Sub msg


type alias Model =
    { baseurl : String
    , errors : List String
    , name : String
    , meta : M.StdMetadata
    , source : String
    , seriestype : I.SeriesType
    , date_index : Int
    , horizon : HorizonModel
    , insertion_dates : Array String
    , editing : Dict String String
    , processedPasted: List String
    , rawPasted: String
    , initialTs: Dict String Entry
    , zoomedTs : Maybe ( Dict String Entry )
    , initialFormula : Dict String (Maybe Float)
    , zoomedFormula : Maybe ( Dict String (Maybe Float) )
    , monotonicCount : Int
    , clipboardclass : String
    , panActive : Bool
    -- show-values for formula
    , components : List Component
    , componentsData: Dict String Series
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotValueData (Result Http.Error String)
    | GotComponents (Result Http.Error String)
    | GotComponentData String (Result Http.Error String)
    | GotMetadata (Result Http.Error String) -- first command fired
    | GotSource (Result Http.Error String)
    | HasCache ( Result Http.Error String )
    | Horizon ModuleHorizon.Msg
    | InputChanged String String
    | SaveEditedData
    | GotEditedData (Result Http.Error String)
    | Paste PasteType
    | InsertionDates (Result Http.Error String)
    | GetLastInsertionDates (Result Http.Error String)
    | GetLastEditedData (Result Http.Error String)
    | DatesFromZoom (List String)
    | NewDragMode Bool
    | CopyNameToClipboard
    | ResetClipboardClass


convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


maxPoints = 1000

msgTooManyPoints =
    """ Too many points to display. Please select a smaller time
    frame or an area on the graph."""


type alias Entry =
    { value : Maybe Float
    , override : Bool
    , edited : Maybe String
    , index : Int
    }

emptyEntry = Entry Nothing False Nothing 0

type alias Component =
    { name: String
    , ctype: String
    }

type alias PasteType =
    { text: String
    , index: String
    }

type alias Series = Dict String (Maybe Float)


type Method
    = GET
    | POST

-- pasting data

textDecoder: JD.Decoder String
textDecoder =
     JD.at [ "detail", "text" ] JD.string


indexDecoder: JD.Decoder String
indexDecoder =
     JD.at [ "detail", "index" ] JD.string


pasteWithDataDecoder : JD.Decoder PasteType
pasteWithDataDecoder =
        JD.map2 PasteType textDecoder indexDecoder


separator raw =
    if String.contains "\r\n" raw then Just "\r\n" -- windows
    else if String.contains "\n" raw then Just "\n" -- unix
    else Nothing


pasteditems : String -> List String
pasteditems raw =
    let
        sep =
            separator raw
    in
    case sep of
        Nothing ->
            [ raw ]
        Just s ->
            String.split s raw


-- series decoder

entryDecoder : JD.Decoder Entry
entryDecoder =
    JD.map4 Entry
        (JD.field "series" (JD.maybe JD.float))
        (JD.field "markers" JD.bool)
        (JD.succeed Nothing)
        (JD.succeed 0)


dataDecoder : JD.Decoder (Dict String Entry)
dataDecoder =
    JD.dict entryDecoder

componentsDecoder: JD.Decoder (List Component)
componentsDecoder =
    JD.list (JD.map2 Component
                (JD.field "name" JD.string)
                (JD.field "type" JD.string))

getPoints: Model -> Cmd Msg
getPoints model =
    if model.seriestype == I.Primary
        then getSeries model GotEditData "supervision" GET model.name
        else getSeries model GotValueData "state" GET model.name


getSeries:  Model -> (Result Http.Error String -> Msg) -> String -> Method -> String  -> Cmd Msg
getSeries model callback apipoint method name =
    let ( from, to ) = Maybe.withDefault ("", "") model.horizon.horizonBounds
    in
    case model.horizon.queryBounds of
        Nothing -> getOrPostData
                    method
                    { baseurl = model.baseurl
                    , name = name
                    , idate = Nothing
                    , callback = callback
                    , nocache = (U.bool2int model.horizon.viewNoCache)
                    , fromdate = from
                    , todate = to
                    , horizon = model.horizon.horizon |> Maybe.andThen
                                (\key-> OD.get key horizons) |> Maybe.map
                          (String.replace "{offset}" (String.fromInt model.horizon.offset))
                    , tzone = model.horizon.timeZone
                    , inferredFreq = model.horizon.inferredFreq
                    , keepnans = True
                    , apipoint = apipoint
                    }
        Just (min, max) -> getOrPostData
                            method
                            { baseurl = model.baseurl
                            , name = name
                            , idate = Nothing
                            , callback = callback
                            , nocache = (U.bool2int model.horizon.viewNoCache)
                            , fromdate = min
                            , todate = max
                            , horizon = Nothing
                            , tzone = model.horizon.timeZone
                            , inferredFreq = model.horizon.inferredFreq
                            , keepnans = True
                            , apipoint = apipoint
                            }


getOrPostData method query =
    case method of
        GET -> getdata query
        POST -> postData query

getComponents: Model -> Cmd Msg
getComponents model =
    Http.get
        { url = (UB.crossOrigin model.baseurl
                    [ "formula-components", model.name ]
                    [] )
        , expect = Http.expectString GotComponents }


getDataComponents: Model -> Cmd Msg
getDataComponents model =
    Cmd.batch ( List.map
                    ( getRelevantComponent model )
                    model.components )



getRelevantComponent : Model -> Component -> Cmd Msg
getRelevantComponent model component =
    if component.ctype /= "auto"
        then
            getSeries
                model
                ( GotComponentData component.name )
                "state"
                GET
                component.name
        else
            getSeries
                model
                ( GotComponentData component.name )
                "eval_formula"
                POST
                component.name


getsource : String -> String -> Cmd Msg
getsource baseurl name =
    Http.get
        { expect = Http.expectString GotSource
        , url = UB.crossOrigin baseurl
              [ "api", "series", "source" ]
              [ UB.string "name" name ]
        }


encodeBodyEvalFormula: String -> String -> String -> JE.Value
encodeBodyEvalFormula formula from to =
    if from == "" || to == ""
        then  JE.object [ ("text", JE.string formula) ]
        else
             JE.object [ ("text", JE.string formula)
                       , ("from_value_date", JE.string from)
                       , ("to_value_date", JE.string to)
                       ]

postData query =
    Http.post
        { url = UB.crossOrigin
                    query.baseurl
                    [ "api", "series", query.apipoint ]
                    []
        , body = Http.jsonBody ( encodeBodyEvalFormula
                                    query.name
                                    query.fromdate
                                    query.todate
                               )
        , expect = Http.expectString query.callback
        }


reindex : Int -> ( String, Entry ) -> ( String, Entry )
reindex increment keyvalue =
    let
        data = Tuple.second keyvalue
    in
    ( Tuple.first keyvalue
    , { data | index = increment }
    )


decorateVanilla : Dict String (Maybe Float) -> Dict String Entry
decorateVanilla values =
    Dict.fromList
        ( List.map
            ( \ (date, value) ->
                ( date
                , {value=value, override=False, edited=Nothing, index=1}))
            ( Dict.toList values ))


addError: Model -> String -> String -> Model
addError model tag error = U.adderror model (tag ++ " -> " ++ error)


restrictOnZoom: Dict String a -> Maybe (String, String) -> Maybe ( Dict String a )
restrictOnZoom ts zoomBounds =
     case zoomBounds of
         Nothing -> Nothing
         Just ( min, max ) -> Just ( Dict.filter
                                    ((\key _ -> (( key >= min ) && ( key <= max ))))
                                    ts )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)

    in
    case msg of
        GotEditData (Ok rawdata) ->
            case JD.decodeString dataDecoder rawdata of
                Ok val ->
                    let
                        indexedval =
                            Dict.fromList
                                <| List.indexedMap reindex (Dict.toList val)
                    in
                    let zoomTs = case model.horizon.zoomBounds of
                                    Nothing -> Nothing
                                    Just ( min, max ) -> Just  (
                                        Dict.filter
                                            (( \k _ -> (( k >= min ) && ( k <= max ))))
                                            indexedval )
                    in
                        U.nocmd { model
                                    | initialTs = indexedval
                                    , zoomedTs = zoomTs
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    indexedval
                                }
                Err err ->
                  U.nocmd ( addError
                                model
                                "got edit data decode"
                                ( JD.errorToString err ))

        GotEditData (Err _) ->
            U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        GotValueData (Ok rawdata) ->
            case JD.decodeString
                    (JD.dict (JD.maybe JD.float))
                    rawdata of
                Ok val -> ( { model | initialFormula = val
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    val
                                    , monotonicCount = model.monotonicCount + 1
                              }
                           , getComponents model )
                Err err -> U.nocmd ( addError
                                        { model | horizon = setStatusPlot model.horizon Failure }
                                        "got value data decode"
                                        ( JD.errorToString err ))

        GotValueData (Err _) ->
            ( { model | horizon = setStatusPlot model.horizon Failure }
            , Cmd.none )

        GotComponents (Ok rawdata) ->
            case JD.decodeString componentsDecoder rawdata of
                Ok val -> let newmodel = { model | components = val}
                          in ( newmodel
                             , getDataComponents newmodel )
                Err err -> U.nocmd
                               { model | errors = model.errors ++ [JD.errorToString err]}

        GotComponents (Err _) -> ( model, Cmd.none )

        GotComponentData name (Ok rawdata) ->
             case JD.decodeString
                    (JD.dict (JD.maybe JD.float))
                    rawdata of
                Ok val ->  let newCD = Dict.insert name val model.componentsData
                           in
                               U.nocmd { model | componentsData = newCD }
                Err err -> U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}

        GotComponentData name (Err _) -> ( model, Cmd.none )

        Horizon hMsg ->
            let ( newModelHorizon, commands ) =  updateHorizon
                                                    ( actionsHorizon model hMsg  )
                                                    hMsg
                                                    model.horizon
            in
            ( { model | horizon = newModelHorizon
                      , zoomedTs = Nothing
                      , zoomedFormula = Nothing}
            , commands )

        InputChanged date rawvalue ->
            let
                value =
                    String.replace "," "." rawvalue

                floatvalue =
                    String.toFloat value

                patchwithvalue validval =
                    let
                        horizonmodel =
                            model.horizon
                        newentry =
                            updateEntry (parseCopyPastedData validval)
                        patched =
                            Dict.update date newentry ( getActiveTs model )
                    in
                    let patchedModel = setOnActiveTs model patched
                    in
                    U.nocmd { patchedModel | editing = Dict.remove
                                                        date
                                                        model.editing }
            in
            case floatvalue of
                Just _ ->
                    patchwithvalue value

                Nothing ->
                    if value == "" -- we read this as a NaN
                    then patchwithvalue ""
                    else U.nocmd { model
                                     | editing = Dict.update date (\_ -> Just value) model.editing
                                 }

        GetLastInsertionDates (Ok rawdates) ->
            case JD.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    let
                        adates = Array.fromList dates
                        newmodel = { model | insertion_dates = adates }
                    in
                    if (Array.length model.insertion_dates) /= (Array.length adates) then
                        ( newmodel
                        , getSeries model GetLastEditedData "supervision" GET model.name
                        )
                    else
                        ( newmodel
                        , patchEditedData model
                        )

                Err err ->
                    doerr "idates decode" <| JD.errorToString err

        GetLastInsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        GetLastEditedData (Ok rawdata) ->
            case JD.decodeString dataDecoder rawdata of
                Ok val ->
                    let
                        indexedval =
                            Dict.fromList
                                <| List.indexedMap reindex (Dict.toList val)
                        patched =
                            Dict.union (getActiveTs model) indexedval

                        newmodel =
                            { model
                                | horizon = updateHorizonFromData model.horizon patched
                            }
                    in
                    ( newmodel
                    , patchEditedData newmodel
                    )

                Err _ ->
                    U.nocmd model

        GetLastEditedData (Err _) ->
            U.nocmd model

        SaveEditedData ->
            ( { model | horizon = ( setStatusPlot model.horizon Loading )}
            , I.getidates model "series" GetLastInsertionDates
            )

        GotEditedData (Ok _) ->
            ( { model | monotonicCount = model.monotonicCount + 1 }
            , Cmd.batch
                  ( getRelevantData model)
            )

        GotEditedData (Err _) ->
            U.nocmd { model | horizon = ( setStatusPlot model.horizon Failure )}

        GotMetadata (Ok result) ->
            case JD.decodeString M.decodemeta result of
                Ok allmeta ->
                   let newmodel = { model | meta = allmeta
                                          , seriestype = if Dict.member "formula" allmeta
                                                            then I.Formula
                                                            else  I.Primary
                                          , monotonicCount = model.monotonicCount + 1
                                  }
                   in
                       ( newmodel
                       , Cmd.batch ( [ getHasCache newmodel
                                     ] ++ (getRelevantData newmodel)))
                Err err ->
                    doerr "gotmeta decode" <| JD.errorToString err

        GotMetadata (Err err) ->
            doerr "gotmeta http" <| U.unwraperror err

        GotSource (Ok rawsource) ->
            case JD.decodeString JD.string rawsource of
                Ok source ->
                    U.nocmd { model | source = source }
                Err err ->
                    doerr "gotsource decode" <| JD.errorToString err

        GotSource (Err err) ->
            doerr "gotsource http" <| U.unwraperror err

        HasCache (Ok rawhascache) ->
            let model_horizon = model.horizon
            in
                U.nocmd { model | horizon =
                            { model_horizon | hasCache = String.startsWith
                                                            "true"
                                                            rawhascache }}

        HasCache (Err error) ->
            doerr "hascache http" <| U.unwraperror error

        Paste payload ->
            let
                newtimeSeries = getPastedDict model payload
            in
            U.nocmd ( setOnActiveTs model newtimeSeries )

        InsertionDates (Ok rawdates) ->
            case JD.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    U.nocmd { model
                                | insertion_dates = Array.fromList dates
                                , date_index = List.length dates - 1
                            }
                Err err ->
                    doerr "idates decode" <| JD.errorToString err

        InsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        DatesFromZoom dates ->
                 let
                    zoomDates = extractZoomDates dates
                    horizonmodel =
                        model.horizon
                    newmodel =
                        case zoomDates of
                            Nothing -> { model | horizon = { horizonmodel | zoomBounds = Nothing}
                                               , zoomedTs = Nothing
                                               , zoomedFormula = Nothing
                                               , monotonicCount = model.monotonicCount + 1
                                        }
                            Just (minDate, maxDate) -> { model | zoomedTs = Just ( newZoom
                                                                                        minDate
                                                                                        maxDate
                                                                                        model.initialTs
                                                                                        model.zoomedTs
                                                                                        model.panActive )
                                                                , zoomedFormula = Just ( newZoom
                                                                                            minDate
                                                                                            maxDate
                                                                                            model.initialFormula
                                                                                            model.zoomedFormula
                                                                                            model.panActive )
                                                                , horizon = { horizonmodel | zoomBounds = Just (minDate, maxDate) }
                                                                , monotonicCount = model.monotonicCount + 1
                                                        }

                 in
                    ( newmodel , Cmd.none )

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }

        CopyNameToClipboard ->
            ( { model | clipboardclass = "bi bi-check2" }
            , Cmd.batch
                [ copyToClipboard model.name
                , T.perform (always (ResetClipboardClass)) (P.sleep 1000)
                ]
            )

        ResetClipboardClass ->
            U.nocmd { model | clipboardclass = "bi bi-clipboard" }


defaultActions newModel =
    getRelevantData newModel

actionsHorizon : Model -> ModuleHorizon.Msg -> HorizonModel -> List (Cmd Msg)
actionsHorizon model msg horizonModel =
    let newModel = { model | horizon = horizonModel}
    in
    case msg of
        ModuleHorizon.FromLocalStorage _ ->
            -- This branch starts the chain of command at initialization
            [(M.getsysmetadata model.baseurl model.name GotMetadata "series")]

        Horizon.HorizonSelected _ -> defaultActions newModel

        Horizon.UpdateOffset _ ->  defaultActions newModel

        Horizon.TimeZoneSelected _ -> defaultActions newModel

        Horizon.InferredFreq _ -> defaultActions newModel

        Horizon.ViewNoCache -> defaultActions newModel


newZoom: String -> String -> Dict String e -> Maybe ( Dict String e ) ->  Bool -> Dict String e
newZoom minDate maxDate initial zoom pan =
    case zoom of
        Nothing -> ( Dict.filter
                        ((\key _ -> ((key >= minDate) && (key <= maxDate))))
                        initial )
        Just zoomTs ->
            ( Dict.filter
                ((\key _ -> ((key >= minDate) && (key <= maxDate))))
                ( if pan
                    then initial
                    else zoomTs) )


getActiveTs: Model -> Dict String Entry
getActiveTs model =
    case model.zoomedTs of
        Nothing -> model.initialTs
        Just zoom -> zoom


setOnActiveTs: Model -> Dict String Entry -> Model
setOnActiveTs model patch =
    case model.zoomedTs  of
        Nothing -> { model | initialTs = patch}
        Just _ -> { model | zoomedTs = Just patch}


getActiveFormula: Model -> Dict String ( Maybe Float)
getActiveFormula model =
    case model.zoomedFormula of
        Nothing -> model.initialFormula
        Just zoom -> zoom


getHasCache : Model -> Cmd Msg
getHasCache model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-has-cache" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString HasCache
        }


getRelevantData : Model -> List (Cmd Msg)
getRelevantData model =
    if model.seriestype == I.Primary
        then
            [ getPoints model
            , I.getidates model "series" InsertionDates
            ]
        else
            [ getPoints model ]


parseCopyPastedData : String -> Maybe String
parseCopyPastedData value =
    case String.toFloat (String.replace "," "." value) of
        Just _ ->
            Just (String.replace "," "." value)

        Nothing ->
            if value == "" then
                Just ""
            else
                Nothing


getPastedDict : Model -> PasteType -> Dict String Entry
getPastedDict model payload =
    let
        newValues =
            List.map parseCopyPastedData (pasteditems payload.text)
        firstIndex =
            Maybe.unwrap
                0
                (\entry -> entry.index)
                (Dict.get (payload.index) ( getActiveTs model ))
        listIndex =
            List.range firstIndex (firstIndex + (List.length newValues) - 1)
        listDates =
            Dict.keys
                (Dict.filter
                     (\_ value -> List.member value.index listIndex)
                     ( getActiveTs model )
                )
        copyPastedDict =
            Dict.fromList <| List.map2 Tuple.pair listDates newValues
    in
    Dict.merge
        (\_ _ dict -> dict)
        (\key _ value dict -> Dict.update key (updateEntry value) dict)
        (\_ _ dict -> dict)
        ( getActiveTs model )
        copyPastedDict
        ( getActiveTs model )


updateEntry : Maybe String -> Maybe Entry -> Maybe Entry
updateEntry value maybeEntry =
    maybeEntry
        |> Maybe.andThen
           (\entry ->
                if (parseCopyPastedData (Maybe.unwrap "" String.fromFloat entry.value)) /= value
                then Just { entry | edited = value }
                else Just { entry | edited = Nothing }
           )


patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let
        tzaware =
            case Dict.get "tzaware" model.meta of
                Just (M.MBool val) -> val
                _ -> False

        patch =
            ( getActiveTs model )
                |> Dict.filter (\_ value -> Maybe.isJust value.edited)
                |> Dict.map (\_ value -> Maybe.withDefault "" value.edited)
    in
    Http.request
        { method = "PATCH"
        , body = Http.jsonBody <| JE.object
                 [ ("name", JE.string model.name )
                 , ("author" , JE.string "webui" )
                 , ("tzaware", JE.bool tzaware )
                 , ("series", encodeEditedData patch )
                 , ("supervision", JE.bool True )
                 , ("tzone", JE.string model.horizon.timeZone)
                 , ("keepnans", JE.bool True)
                 ]
        , headers = [ ]
        , timeout = Nothing
        , tracker = Nothing
        , url = UB.crossOrigin model.baseurl
                [ "api", "series", "state" ] [ ]
        , expect = Http.expectString GotEditedData
        }


encodeEditedData : Dict String String -> JE.Value
encodeEditedData editedData =
    JE.dict
        identity
        (\value ->
            if value == ""
            then JE.null
            else JE.float (Maybe.withDefault 0.0 (String.toFloat value))
        )
        editedData


permaLink: Model -> H.Html Msg
permaLink model =
    H.a
        [ HA.href ( UB.crossOrigin
                        model.baseurl
                        ["tseditor"]
                        ( queryNav model model.name ))
        , HA.target "_blank"
        ]
        [ H.text "Permalink"]


viewsavebutton : PlotStatus -> Dict String Entry -> H.Html Msg
viewsavebutton plotstatus patch =
    let
        status =
            case plotstatus of
                Loading ->
                    "Saving ... please wait"
                Success ->
                    "Save"
                _ ->
                    "Saving failed. Are you editing a forecast ?"
    in
    H.div
        [ HA.class "button-save-data" ]
        [ H.button
              [ HA.class (if Dict.isEmpty patch then "invisible" else "greenbutton")
              , HA.attribute "type" "button"
              , HE.onClick SaveEditedData
              , HA.disabled (plotstatus == Loading)
              ]
              [ H.text status ]
        ]


divSaveDataTable : Dict String Entry -> H.Html Msg
divSaveDataTable filtredDict =
    let
        row : (String, Entry) -> H.Html Msg
        row (date, entry) =
            H.tr
                [ ]
                [ H.td [ ] [ H.text date ]
                , H.td [ ] [ H.text (Maybe.withDefault "" entry.edited)]
                ]
        classlist =
            [ HA.class "save-data-table" ]
    in
    if Dict.isEmpty filtredDict then
        H.div classlist [ ]
    else
        H.div
            classlist
            [ H.table
                  [ HA.class "table-style" ]
                  [ H.thead
                        [ ]
                        [ H.tr
                              [ ]
                              [ H.th
                                    [ HA.scope "col" ]
                                    [ H.text "Dates" ]
                              , H.th
                                  [ HA.scope "col" ]
                                  [ H.text "Values" ]
                              ]
                        ]
                  , H.tbody
                      [ HA.class "row-green" ]
                      (List.map row (Dict.toList filtredDict))
                  ]
            ]


editTable : Model -> H.Html Msg
editTable model =
    let
        node = H.node "eval-js"
            [ HA.attribute
                  "myjs"
                  ("applyCopyPaste(" ++ String.fromInt model.monotonicCount ++ ");")
            ]
            [ ]
        class = HA.class "data-table"
    in
    if Dict.isEmpty ( getActiveTs model )
    then H.div [ class ][ ]
    else
        if Dict.size ( getActiveTs model ) > maxPoints
        then H.div
            [ class ]
            [ H.text msgTooManyPoints
            , node
            ]
    else
        H.div
            [ class ]
            [ H.table
                  [ HA.class "table-style" ]
                  [ H.thead [ ]
                        [ H.tr [ ]
                              [ H.th
                                    [ HA.scope "col" ]
                                    [ H.text "Dates" ]
                              , H.th
                                  [ HA.scope "col" ]
                                  [ H.text "Values" ]
                              ]
                        ]
                  , H.tbody [ ]
                      <| List.map (viewrow model) (Dict.toList ( getActiveTs model ))
                  ]
            , node
            ]


viewRelevantTable: Model -> H.Html Msg
viewRelevantTable model =
    if model.seriestype == I.Primary
        then  viewedittable model
        else viewValueTable model


viewValueTable: Model -> H.Html Msg
viewValueTable model =
    H.table
        [HA.class "table talbe-sm editor-values-table"]
          [ headerShowValue model
          , bodyShowValue model ]


bodyShowValue: Model -> H.Html Msg
bodyShowValue model =
    if (List.length (Dict.toList ( getActiveFormula model ))) < maxPoints
    then
        H.tbody
            []
            ( List.map
                ( buildRow model )
                ( datesValue model ))
    else
        H.p
            []
            [ H.text msgTooManyPoints ]


datesValue: Model -> List String
datesValue model =
    List.sort
    ( Set.toList
        ( Set.union
             ( datesFormula model )
             ( datesComponents model )))


datesFormula: Model -> Set String
datesFormula model =
    Set.fromList
        ( List.map
            (\ (date, _) -> date)
            ( Dict.toList ( getActiveTs model ) ))


datesComponents: Model -> Set String
datesComponents model =
    List.foldl
        Set.union
        Set.empty
        ( List.map
            (datesComponent model)
            model.components )


datesComponent: Model -> Component -> Set String
datesComponent model comp =
    let allDates = ( Dict.keys
                    ( Maybe.withDefault
                            Dict.empty
                            ( Dict.get
                                comp.name
                                model.componentsData )))
        bounds = getFromToDates model.horizon
    in
    case bounds of
        Nothing -> Set.fromList allDates
        Just ( min, max ) -> Set.fromList
                                ( List.filter
                                (\ date -> (date >= min) && (date <= max))
                                allDates )


buildRow : Model -> String -> H.Html Msg
buildRow model date =
    H.tr
        []
        ( List.append
            [ H.th
                [HA.class "editor-values-table-dates"]
                [ H.text date ]
            , H.td [] [ H.text ( printValue
                                    ( Maybe.withDefault
                                          Nothing
                                          ( Dict.get
                                            date
                                            ( getActiveFormula model ) )
                                    ))
                        ]]
            ( addComponentCells model date ) )


headerShowValue: Model -> H.Html Msg
headerShowValue model =
    H.thead
        []
        [ H.tr
            []
            ( [ H.th [ HA.class "editor-values-table-dates" ] []
              , H.th
                    [ HA.class "editor-values-table-series" ]
                    [ H.a
                        []
                        [ H.text model.name ]
                    ]
              ] ++ List.map ( buildLink model ) model.components  )
        ]


queryNav: Model -> String -> List UB.QueryParameter
queryNav model name =
    let bounds = getFromToDates model.horizon
        base = UB.string "name" name
    in
    case bounds of
        Nothing -> [ base ]
        Just ( min, max )-> [ base
                            , UB.string "startdate" min
                            , UB.string "enddate" max
                            ]


buildLink: Model -> Component -> H.Html Msg
buildLink model comp =
    H.th
        [ HA.class "editor-values-table-series" ]
        ( if comp.ctype /= "auto"
            then
                [ H.a
                    [ HA.href ( UB.crossOrigin model.baseurl
                                    [ "tseditor" ]
                                    ( queryNav model comp.name ))]
                    [ H.text comp.name ]]
            else
                [ H.p [] [ H.text comp.name ]]
        )


addComponentCells: Model -> String -> List (H.Html Msg)
addComponentCells model date =
    List.map
        ( \ comp -> H.td [] [H.text ( printValue
                               ( Maybe.withDefault
                                   Nothing
                                   ( Dict.get
                                        date
                                        ( Maybe.withDefault
                                            Dict.empty
                                            ( Dict.get
                                                comp.name
                                                model.componentsData)))))])
        model.components

printValue: Maybe Float -> String
printValue value =
    case value of
        Nothing -> ""
        Just val -> String.fromFloat val


viewedittable : Model -> H.Html Msg
viewedittable model =
    let
        filtredDict = Dict.filter
            (\_ entry -> Maybe.isJust entry.edited)
            ( getActiveTs model )
    in
    H.div
        [ HA.class "tables" ]
        [ viewsavebutton model.horizon.plotStatus filtredDict
        , editTable model
        , divSaveDataTable filtredDict
        ]


viewrow : Model -> ( String, Entry ) -> H.Html Msg
viewrow model ( date, entry ) =
    let
        editing =
            Dict.get date model.editing
        data =
            case editing of
                Just edited -> edited
                Nothing ->
                    if Maybe.isJust entry.edited
                    then Maybe.withDefault "" entry.edited
                    else Maybe.unwrap "" String.fromFloat entry.value

        rowstyle =
            case editing of
                Just _ -> "row-invalid"
                Nothing ->
                    if Maybe.isJust entry.edited
                    then "row-editing"
                    else if entry.override
                         then "row-override"
                         else if Maybe.isNothing entry.value
                              then "row-nan"
                              else ""
    in
    H.tr [ ]
        [ H.td
              [ HA.class rowstyle]
              [ H.text <| String.replace "T" " " date ]
        , H.td
            [ ]
            [ H.input
                  [ HA.class ("pastable " ++ rowstyle)
                  , HA.placeholder "enter your value"
                  , HA.value data
                  , HE.onInput (InputChanged date)
                  , HA.attribute "index" date
                  , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                  ]
                  [ ]
            ]
        ]


statusText : ModuleHorizon.PlotStatus -> String
statusText plotStatus =
    if plotStatus == None then
        "Init"
    else if plotStatus == Loading then
        "Loading ..."
    else if plotStatus == Success then
        ""
    else
        "Failure"


isEmpty: Model -> Bool
isEmpty model =
    if model.seriestype == I.Primary
    then Dict.isEmpty ( getActiveTs model )
    else Dict.isEmpty ( getActiveFormula model )


getTs: Model -> ( List String, List ( Maybe Float ))
getTs model =
    if model.seriestype == I.Primary
    then
        (( Dict.keys ( getActiveTs model ) )
        , ( List.map (\x -> x.value) (Dict.values ( getActiveTs model ) )))
    else
        (( Dict.keys ( getActiveFormula model ) )
        , ( Dict.values ( getActiveFormula model ) ))


view : Model -> H.Html Msg
view model =
    let
        maybeMedian = medianValue (Dict.keys ( getActiveTs model ))
        ( dates, values ) = getTs model
        dragMode =
            if model.panActive
            then "pan"
            else "zoom"
    in
    H.div
        [ HA.class "main-content" ]
        [ H.span [ HA.class "action-container" ]
              <| I.viewactionwidgets model convertMsg False "Series Editor"
        , I.viewtitle model maybeMedian CopyNameToClipboard
        , H.div
            [ HA.class "status-plot" ]
            [ if model.horizon.plotStatus == None
              then H.text "The graph is loading, please wait"
              else if isEmpty model && (model.horizon.plotStatus == Success)
                   then H.text """It seems there is no data to display in this
                                interval, select another one."""
                   else H.text ""
            ]
        , I.viewgraph
            model.name
            dates
            values
            { defaultLayoutOptions | dragMode = dragMode }
            defaultoptions
        , permaLink model
        , viewRelevantTable model
        , H.div [] ( List.map (\ err -> H.p [] [H.text err]) model.errors)

        ]


type alias Input =
    { baseurl : String
    , name : String
    , min: String
    , max: String
    }


init : Input -> ( Model, Cmd Msg )
init input =
     ({ baseurl = input.baseurl
                    , errors = [ ]
                    , name = input.name
                    , meta = Dict.empty
                    , source = ""
                    , seriestype = I.Primary
                    , date_index = 0
                    , horizon = initHorizon input.min input.max Loading
                    , editing = Dict.empty
                    , insertion_dates = Array.empty
                    , processedPasted = [ ]
                    , monotonicCount = 0
                    , rawPasted = ""
                    , initialTs = Dict.empty
                    , zoomedTs = Nothing
                    , initialFormula = Dict.empty
                    , zoomedFormula = Nothing
                    , clipboardclass = "bi bi-clipboard"
                    , panActive = False
                    , components = []
                    , componentsData = Dict.empty
                    }
    , getsource input.baseurl input.name -- The chain of command is triggerd by "FromLocalStorage" Msg
    )


main : Program Input Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
          \_ -> Sub.batch
                [ dateInInterval DatesFromZoom
                , panActive NewDragMode
                , loadFromLocalStorage
                    (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
                ]
        }

