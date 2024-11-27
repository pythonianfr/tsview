port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Dateinterval exposing (medianValue)
import Dict exposing (Dict)
import Set exposing (Set)
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , ZoomFromPlotly
    , initHorizon
    , getFromToDates
    , getFetchBounds
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
import List.Extra as List
import List.Statistics as Stat
import Maybe.Extra as Maybe
import Metadata as M
import OrderedDict as OD
import Plotter exposing
    ( defaultLayoutOptions
    , defaultTraceOptions
    , defaultConfigOptions
    , getdata
    , scatterplot
    , serializedPlotArgs
    )
import Process as P
import Round
import Url.Builder as UB
import Util as U
import Json.Encode as JE
import Task as T

port copyToClipboard : String -> Cmd msg
port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg
port panActive : (Bool -> msg) -> Sub msg
port copySignal: (Bool -> msg) -> Sub msg


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.map toKey (JD.field "key" JD.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            NoAction
        _ ->
            ControlKey keyValue


type alias Model =
    { baseurl : String
    , name : String
    , meta : M.StdMetadata
    , source : String
    , seriestype : I.SeriesType
    , horizon : HorizonModel
    -- data
    , insertion_dates : Array String
    , initialTs: Dict String Entry
    , zoomedTs : Maybe ( Dict String Entry )
    , statistics: Statistics
    , roundStat: Int
    -- technical
    , errors : List String
    , rawPasted: String
    , processedPasted: List String
    , initialCommands : Cmd Msg
    , monotonicCount : Int
    , clipboardclass : String
    , panActive : Bool
    -- cells interactivity
    , forceDraw : Bool
    , firstSelected : Maybe Int
    , dragOn: Bool
    , lastValids: List Int
    , slope: Maybe String
    , intercept: Maybe String
    -- show-values for formula
    , initialFormula : Dict String (Maybe Float)
    , zoomedFormula : Maybe ( Dict String (Maybe Float) )
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
    | SwitchForceDraw
    | InputChanged String String
    | SaveEditedData
    | CancelEdition
    | Correction Parameter
    | GotEditedData (Result Http.Error String)
    | Paste PasteType
    | SelectRow Int
    | DeselectAll
    | Drag DragMode
    | CopySelection
    | CopyFromBrowser Bool
    | ControlKey String
    | NoAction
    | FillNas Int
    | FillAll
    | InsertionDates (Result Http.Error String)
    | GetLastInsertionDates (Result Http.Error String)
    | GetLastEditedData (Result Http.Error String)
    | FromZoom ZoomFromPlotly
    | NewDragMode Bool
    | CopyNameToClipboard
    | ResetClipboardClass


convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


type Parameter =
    Slope String
    | Intercept String


type alias Statistics =
    { start : Maybe String
    , end: Maybe String
    , min: Maybe Float
    , max: Maybe Float
    , mean: Maybe Float
    , median: Maybe Float
    }

emptyStat =
    Statistics
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

maxPoints = 1000

msgTooManyPoints nbPoints =
    H.text
        <|  """ Too many points to display. ("""
            ++ String.fromInt nbPoints
            ++ """) Please select a smaller time
            frame or an area on the graph."""


type alias Entry =
    { value : Maybe Float
    , override : Bool
    , edited : Edited
    , index : Int
    , selected: Bool
    }

emptyEntry = Entry
                Nothing
                False
                NoEdition
                0
                False

type Edited =
    Edition Float
    | NoEdition
    | Deletion
    | Error String


type alias Component =
    { name: String
    , ctype: String
    }

type alias PasteType =
    { text: String
    , index: String
    }

type DragMode =
    On
    | Off

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
    JD.map5 Entry
        (JD.field "series" (JD.maybe JD.float))
        (JD.field "markers" JD.bool)
        (JD.succeed NoEdition)
        (JD.succeed 0)
        (JD.succeed False)

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
    let ( start, end ) = getFetchBounds model.horizon
    in
    getOrPostData
        method
        { baseurl = model.baseurl
        , name = name
        , idate = Nothing
        , callback = callback
        , nocache = (U.bool2int model.horizon.viewNoCache)
        , fromdate = start
        , todate = end
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
                    [ "formula-components" ]
                    [ UB.string "name" model.name ] )
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
                        zoomTs = case model.horizon.zoomBounds of
                                    Nothing -> Nothing
                                    Just ( min, max ) -> Just  (
                                        Dict.filter
                                            (( \k _ -> (( k >= min ) && ( k <= max ))))
                                            indexedval )
                        statistics = getStatistics ( onlyValues indexedval )
                    in
                        ( setupNas
                            ({ model
                                    | initialTs = indexedval
                                    , zoomedTs = zoomTs
                                    , statistics = statistics
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    indexedval
                            })
                        ,
                        Cmd.none )
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
                                                    hMsg
                                                    convertMsg
                                                    model.horizon
            in
            let newModel =  {model | horizon = newModelHorizon
                                   , monotonicCount = model.monotonicCount + 1
                                   , zoomedTs = Nothing
                                   , zoomedFormula = Nothing}
            in
            let default = ( newModel, commands )
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ -> ( { newModel | forceDraw = False
                                                      , firstSelected = Nothing
                                           }
                                         , commands )
                ModuleHorizon.FromLocalStorage _ ->
                    -- we want to fire the commands AFTER getting the metadata:
                    -- we store these commands in the model -.-
                    ( { newModel | initialCommands = commands }
                    , Cmd.batch ([ M.getsysmetadata
                                        model.baseurl
                                        model.name
                                        GotMetadata
                                        "series"]))
                ModuleHorizon.Fetch _ -> ( newModel
                                         , Cmd.batch ([ commands ]
                                         ++ getRelevantData newModel ))

        SwitchForceDraw ->
            ( flipForce model , Cmd.none)

        InputChanged date rawvalue ->
            let
                edition = parseInput rawvalue
            in
            ( setupNas
                <| setOnActiveTs model
                        <| patchWithValue
                            (getActiveTs model)
                            ( date, edition )
            , Cmd.none )

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

        CancelEdition ->
            ( setupNas
                (setOnActiveTs model
                    (Dict.map
                        (\ _ e -> { e | edited = NoEdition })
                        ( getActiveTs model ))
                )
            , Cmd.none
            )

        Correction param ->
            case param of
                Slope value ->  U.nocmd { model | slope = Just value}
                Intercept value -> U.nocmd { model | intercept = Just value}

        GotEditedData (Ok _) ->
            ( { model | monotonicCount = model.monotonicCount + 1
                      , slope = Nothing
                      , intercept = Nothing
              }
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
                       , Cmd.batch ([ getHasCache newmodel
                                    , model.initialCommands
                                    ])
                       )
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
                newtimeSeries = applyPastedDict model payload
            in
            U.nocmd ( setOnActiveTs model newtimeSeries )

        SelectRow index ->
            let transformed = Dict.map
                                (if model.dragOn
                                    then
                                        (selectContiguous model index)
                                    else
                                     ( \ _ v ->
                                        { v | selected = if v.selected
                                                        then if ( v.index == index )
                                                                then False
                                                                else True
                                                        else if ( v.index == index )
                                                                then True
                                                                else False
                                        }
                                    )
                                )
                                ( getActiveTs model )
                newmodel = setOnActiveTs model transformed
                ( _, first ) = firstSelected ( Dict.values (getActiveTs newmodel))
            in
                U.nocmd { newmodel | firstSelected = first }

        DeselectAll ->
             let transformed = Dict.map
                                ( \ _ v -> { v | selected = False })
                                ( getActiveTs model )
                 newmodel = setOnActiveTs model transformed
              in
                U.nocmd { newmodel | firstSelected = Nothing }

        CopySelection ->
            let selectedValues = getSelectedValues model
                concatened = String.join "\n" selectedValues
            in
            ( model
            , Cmd.batch [ T.perform identity (T.succeed DeselectAll)
                        , copyToClipboard concatened
                        ]
            )

        CopyFromBrowser _->
            ( model, T.perform identity ( T.succeed CopySelection ))

        FillNas indexLastValue ->
            let
                lastValue = getLastValue
                                ( getActiveTs model )
                                indexLastValue
            in
                ( setOnActiveTs
                    model
                        <|Dict.union
                            (fillNas
                                ( getActiveTs model )
                                lastValue
                                ( indexLastValue )
                            )
                            ( getActiveTs model )
                , Cmd.none )

        FillAll ->
            ( setOnActiveTs
                model
                <| fillAllNas
                    ( getActiveTs model )
                    model.lastValids
            , Cmd.none
            )

        NoAction -> U.nocmd model

        ControlKey key ->
            if key == "Escape"
                then
                    ( model
                    , T.perform identity (T.succeed DeselectAll)
                    )
            else
            if key == "Delete"
                then

                    ( setupNas
                        ( deleteSelectedValues model )
                    , T.perform identity (T.succeed DeselectAll)
                    )

                else U.nocmd model

        Drag mode ->
            case mode of
                On -> U.nocmd { model | dragOn = True }
                Off -> U.nocmd  { model | dragOn = False }

        InsertionDates (Ok rawdates) ->
            case JD.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    U.nocmd { model
                                | insertion_dates = Array.fromList dates
                            }
                Err err ->
                    doerr "idates decode" <| JD.errorToString err

        InsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        FromZoom dates ->
                 let
                    zoomDates = (extractZoomDates dates).x
                    horizonmodel =
                        model.horizon
                    newmodel =
                        case zoomDates of
                            Nothing -> { model | horizon = { horizonmodel | zoomBounds = Nothing}
                                               , zoomedTs = Nothing
                                               , zoomedFormula = Nothing
                                               , monotonicCount = model.monotonicCount + 1
                                               , forceDraw = False
                                               , statistics = getStatistics ( onlyValues  model.initialTs )
                                        }
                            Just (minDate, maxDate) -> let zoomedTs =  newZoom
                                                                            minDate
                                                                            maxDate
                                                                            model.initialTs
                                                                            model.zoomedTs
                                                                            model.panActive
                                                        in
                                                            { model | zoomedTs = Just zoomedTs
                                                            , zoomedFormula = Just ( newZoom
                                                                                        minDate
                                                                                        maxDate
                                                                                        model.initialFormula
                                                                                        model.zoomedFormula
                                                                                        model.panActive )
                                                            , horizon = { horizonmodel | zoomBounds = Just (minDate, maxDate) }
                                                            , monotonicCount = model.monotonicCount + 1
                                                            , statistics = getStatistics ( onlyValues  zoomedTs )
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


patchWithValue: Dict String Entry -> (String , Edited) -> Dict String Entry
patchWithValue series (date, edition) =
    let
        newentry =
            updateEntry edition
    in
        Dict.update
            date
            newentry
            series


flipForce: Model -> Model
flipForce model =
    { model | forceDraw = not model.forceDraw
            , monotonicCount = model.monotonicCount + 1
    }


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


onlyValues: Dict String Entry -> Dict String Float
onlyValues series =
    Dict.fromList
        <| List.concat
            <| List.map
                (\ (k, e) -> case e.value of
                                Nothing -> []
                                Just val -> [(k, val)]
                )
                (Dict.toList series)


getStatistics: Dict String Float -> Statistics
getStatistics series =
    let dates = List.sort ( Dict.keys series )
        values = List.sort ( Dict.values series )
    in
        { start = List.head dates
        , end = List.last dates
        , min = Stat.minimum values
        , max = Stat.maximum values
        , mean = Stat.mean values
        , median = Stat.median values
        }


getCurrentValue: Entry -> Maybe Float
getCurrentValue entry =
    case entry.edited of
        Edition value -> Just value
        Deletion -> Nothing
        NoEdition -> entry.value
        Error _ -> Nothing


getSelectedValues: Model -> List String
getSelectedValues model =
    List.map
        (\ e ->  case (getCurrentValue e) of
                    Nothing -> ""
                    Just val -> String.fromFloat val
        )
        <| List.filter
            (\ e -> e.selected )
            ( Dict.values ( getActiveTs model ))


deleteSelectedValues: Model -> Model
deleteSelectedValues model =
    setOnActiveTs
        model
        <| Dict.map
                (\ k e ->  if e.selected
                            then
                                { e | edited = Deletion}
                            else e
                )
                ( getActiveTs model )


setupNas: Model -> Model
setupNas model =
    { model | lastValids = findLastValid
                            ( Dict.toList
                                ( getActiveTs model )
                            )
                            False
                            []
    }


findLastValid: List (String,  Entry) -> Bool -> List Int -> List Int
findLastValid series previousIsValue found =
    case series of
        [] -> found
        (k, val) :: xs ->
            if previousIsValue
                then
                if getCurrentValue val == Nothing
                    then  List.concat [ [ val.index - 1]
                                       , ( findLastValid xs False found )
                                       ]
                    else ( findLastValid xs True found )
                else
                    if getCurrentValue val == Nothing
                        then ( findLastValid xs False found )
                        else ( findLastValid xs True found )


fillNas: Dict String  Entry  -> Float -> Int -> Dict String Entry
fillNas series lastValue indexLastValue =
    Dict.fromList
        <|recFillNas
                ( List.filter
                    (\ (_, e) -> e.index > indexLastValue)
                    ( Dict.toList series )
                )
                lastValue
                []


recFillNas: List (String,  Entry)  -> Float -> List (String,  Entry)  -> List (String,  Entry)
recFillNas series lastValue result =
    case series of
        [] -> result
        (k, entry) :: xs ->
            case getCurrentValue entry of
                Just _ -> result
                Nothing -> List.concat[
                            [(k, {entry | edited = Edition lastValue})]
                            , recFillNas
                                xs
                                lastValue
                                result
                            ]


fillAllNas : Dict String  Entry -> List Int -> Dict String  Entry
fillAllNas series idxNa =
    case idxNa of
        [] -> series
        x :: xs ->
            let lastValue = getLastValue series x
            in
                Dict.union
                    ( fillNas
                        series
                        lastValue
                        x
                    )
                    ( fillAllNas
                        series
                        xs
                    )


getLastValue: Dict String  Entry -> Int -> Float
getLastValue series indexNa =
    let
        entry = Maybe.withDefault
                    emptyEntry
                        <| List.head
                            <|Dict.values
                                <|Dict.filter
                                    ( \ _ e -> e.index == indexNa  )
                                    series
    in
        Maybe.withDefault 0 ( getCurrentValue entry )


selectContiguous: Model -> Int -> ( String -> Entry -> Entry )
selectContiguous model index =
    case model.firstSelected of
        Nothing ->  ( \ _ v ->
                        { v | selected = v.index == index }
                    )
        Just first ->
            if index < first
                then   ( \ _ v ->
                            { v | selected =
                                v.selected
                                || ( ( v.index >= index ) && ( v.index <= first ))
                            }
                        )
                else
                        ( \ _ v ->
                            { v | selected =
                                v.selected
                                || ( ( v.index <= index ) && ( v.index >= first ))
                            }
                        )

getRelevantData : Model -> List (Cmd Msg)
getRelevantData model =
    if model.seriestype == I.Primary
        then
            [ getPoints model
            , I.getidates model "series" InsertionDates
            ]
        else
            [ getPoints model ]


parseInput : String -> Edited
parseInput value =
    if String.endsWith "." value
    then
        Error value
    else
        if value == ""
            then Deletion
            else
                case String.toFloat
                        <| String.replace
                                ","
                                "."
                                value
                of
                    Just val ->
                        Edition val
                    Nothing ->
                        Error value


applyPastedDict : Model -> PasteType -> Dict String Entry
applyPastedDict model payload =
    let
        newValues =
            List.map
                parseInput
                (pasteditems payload.text)
        firstIndex =
            Maybe.unwrap
                0
                (\entry -> entry.index)
                (Dict.get
                    (payload.index)
                    ( getActiveTs model )
                )
        listIndex =
            List.range
                firstIndex
                (firstIndex + (List.length newValues) - 1)
        listDates =
            Dict.keys
                (Dict.filter
                     (\_ value -> List.member value.index listIndex)
                     ( getActiveTs model )
                )
        copyPastedDict = buildCopyPasteDict
                            listDates
                            newValues
    in
    Dict.merge
        (\_ _ dict -> dict)
        (\key _ value dict -> Dict.update
                                key
                                (updateEntry value)
                                dict
        )
        (\_ _ dict -> dict)
        ( getActiveTs model )
        copyPastedDict
        ( getActiveTs model )


updateEntry : Edited -> Maybe Entry -> Maybe Entry
updateEntry value maybeEntry =
    case maybeEntry of
        Nothing -> Nothing
        Just entry ->
             Just { entry | edited = value }


buildCopyPasteDict : List String -> List Edited -> Dict String Edited
buildCopyPasteDict listDates newValues =
    Dict.fromList <| List.map2
                        Tuple.pair
                        listDates
                        newValues

patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let
        tzaware =
            case Dict.get "tzaware" model.meta of
                Just (M.MBool val) -> val
                _ -> False

        patch = filterAndConvert
                    <| Dict.map
                        ( \ _ e -> e.edited )
                        ( currentDiff model )
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


filterAndConvert: Dict String Edited -> Dict String ( Maybe Float )
filterAndConvert editedData =
    Dict.fromList
        <| List.concat
            <| List.map
                    (\ (k, e) -> case e of
                                    NoEdition -> []
                                    Error _ -> []
                                    Deletion -> [(k, Nothing)]
                                    Edition val -> [(k, Just val)]
                    )
                    ( Dict.toList editedData )


encodeEditedData : Dict String (Maybe Float) -> JE.Value
encodeEditedData editedData =
    JE.dict
        identity
        ( \value ->
            case value of
                Nothing -> JE.null --deletion
                Just val -> JE.float val
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
         (if Dict.isEmpty patch
            then []
            else
                [ H.button
                  [ HA.class  "bluebutton"
                  , HA.attribute "type" "button"
                  , HE.onClick CancelEdition
                  , HA.disabled (plotstatus == Loading)
                  ]
                  [ H.text "Cancel" ]
                , H.button
                  [ HA.class  "greenbutton"
                  , HA.attribute "type" "button"
                  , HE.onClick SaveEditedData
                  , HA.disabled (plotstatus == Loading)
                  ]
                  [ H.text status ]
                ]
         )


divSaveDataTable : Dict String Entry -> H.Html Msg
divSaveDataTable filtredDict =
    let
        row : (String, Entry) -> H.Html Msg
        row (date, entry) =
            H.tr
                [ ]
                [ H.td [ ] [ H.text date ]
                , H.td [ ] [ H.text (case entry.edited of
                                        Edition val -> String.fromFloat val
                                        _ -> ""
                                    )
                            ]
                ]
    in
    if Dict.isEmpty filtredDict then
        H.div [] []
    else
        H.div
            []
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



msgTooManyPointsWithButton: Int -> H.Html Msg
msgTooManyPointsWithButton nbPoints =
    H.div
        []
        [ msgTooManyPoints nbPoints
        , H.button
            [ HE.onClick SwitchForceDraw
            , HA.type_ "button"
            , HA.class "btn btn-warning"]
            [ H.text "Show anyway"]
        ]


editTable : Model -> H.Html Msg
editTable model =
    let
        nodeTriggerPastable = H.node "eval-js"
            [ HA.attribute
                  "myjs"
                  ( "applyCopyPaste("
                     ++ String.fromInt model.monotonicCount
                     ++ ");")
            ]
            [ ]
        class = HA.class "data-table"
        buttonFillAll = if model.lastValids /= []
                            then H.button
                                    [ HA.class "bluebutton"
                                    , HE.onClick FillAll
                                    ]
                                    [ H.text "Fill all Nas ↓" ]
                            else
                                H.div [] []
    in
    if Dict.isEmpty ( getActiveTs model )
    then H.div [ class ][ ]
    else
        let nbPoints = (Dict.size ( getActiveTs model ))
        in
        if nbPoints > maxPoints
            && not model.forceDraw
        then H.div
            [ class ]
            [ msgTooManyPointsWithButton nbPoints
            -- for some reason an existing input help
            -- to apply the method from a blank page
            , H.input
                [ HA.class "pastable"
                , HA.hidden True]
                []
            , nodeTriggerPastable
            ]
    else
        H.div
            [ class ]
            [ buttonFillAll
            , H.table
                  [ HA.class "table-style" ]
                  [ H.thead [ ]
                        [ H.tr [ ]
                              [ H.th
                                    [ HA.scope "col" ]
                                    [ H.text "Dates" ]
                              , H.th
                                  [ HA.scope "col" ]
                                  [ H.text "Values" ]
                              , H.th
                                  [ HA.class "control-col" ]
                                  [ ]
                              , H.th
                                  [ HA.class "control-col" ]
                                  [ ]
                              ]
                        ]
                  , H.tbody [ ]
                      <| List.map
                            (viewrow model)
                            (Dict.toList ( getActiveTs model ))
                  ]
            , nodeTriggerPastable
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
    let nbPoints = List.length (Dict.toList ( getActiveFormula model ))
    in
    if nbPoints < maxPoints ||
       model.forceDraw
    then
        H.tbody
            []
            ( List.map
                ( buildRow model )
                ( datesValue model ))
    else
        H.p
            []
            [ msgTooManyPointsWithButton nbPoints ]


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


currentDiff: Model -> Dict String Entry
currentDiff model =
    Dict.map
        (\ _ e -> { e | edited = ( linearCorrection model e.edited )})
        ( Dict.filter
            (\_ entry -> entry.edited  /= NoEdition )
            ( getActiveTs model )
        )


diffToFloat: List Entry -> List ( Maybe Float )
diffToFloat entries =
    List.map
        entryToFloat
        entries


entryToFloat: Entry -> Maybe Float
entryToFloat entry =
    case entry.edited of
        NoEdition -> Nothing
        Error _ -> Nothing
        Deletion -> Nothing
        Edition edit -> Just edit


viewedittable : Model -> H.Html Msg
viewedittable model =
    let
        filtredDict = currentDiff model
    in
    H.div
        [ HA.class "tables" ]
        [ viewsavebutton model.horizon.plotStatus filtredDict
        , editTable model
        , H.div
            [HA.class "save-data-table"]
            [ if List.length ( Dict.toList filtredDict ) /= 0
                then divLinearCorrection model
                else H.div [] []
            , divSaveDataTable filtredDict
            ]
        ]


divLinearCorrection: Model -> H.Html Msg
divLinearCorrection model =
    H.div
        [ HA.class "linear-correction"]
        [ H.text "Y = "
        , H.input
            [ HA.class "correction"
            , case  model.slope of
                Nothing ->
                    HA.placeholder "1"
                Just slope ->
                   HA.value slope
           , HE.onInput (\ s ->  Correction (Slope s) )
           ]
           []
        , H.text "X + "
        , H.input
            [ HA.class "correction"
            , case  model.intercept of
                Nothing ->
                    HA.placeholder "0"
                Just intercept ->
                   HA.value intercept
           , HE.onInput (\ s ->  Correction (Intercept s) )
           ]
           []
        ]


linearCorrection: Model -> Edited -> Edited
linearCorrection model value =
    case value of
        NoEdition -> NoEdition
        Deletion -> Deletion
        Error s -> Error s
        Edition v ->
            let a = case model.slope of
                        Nothing -> Nothing
                        Just slope ->
                            case String.toFloat slope of
                                Nothing -> Nothing
                                Just s -> Just s
                b = case model.intercept of
                        Nothing -> Nothing
                        Just inter ->
                            case String.toFloat inter of
                                Nothing -> Nothing
                                Just i -> Just i
            in
                case a of
                    Nothing ->
                        case b of
                            Nothing -> Edition v
                            Just inter -> Edition ( v + inter )
                    Just slope ->
                        case b of
                            Nothing -> Edition ( v * slope )
                            Just inter ->
                                Edition ( v * slope + inter )


firstSelected: List Entry -> ( List Entry, Maybe Int)
firstSelected entries =
    case entries of
        [] -> ( [], Nothing )
        x::xs ->
            if x.selected
                then ( [], Just x.index )
                else firstSelected xs


viewrow : Model -> ( String, Entry ) -> H.Html Msg
viewrow model ( date, entry ) =
    let
        value =
            case entry.edited of
                Edition v -> String.fromFloat v
                Error s -> s
                Deletion -> ""
                NoEdition ->
                    Maybe.unwrap
                        ""
                        String.fromFloat
                        entry.value
        rowstyle =
            case entry.edited of
                Edition _ -> "row-editing"
                Deletion -> "row-editing"
                Error _ -> "row-invalid"
                NoEdition ->
                    if entry.override
                     then "row-override"
                     else if Maybe.isNothing entry.value
                          then "row-nan"
                          else ""

        isFirstSelected = case model.firstSelected of
                            Nothing -> False
                            Just first -> if entry.index == first
                                            then True
                                            else False
    in
    H.tr
        ([ HA.class "row-edit"
        , if entry.selected
            then HA.class "selected"
            else HA.class ""
        , HE.onDoubleClick (SelectRow entry.index)
        , HE.onMouseDown (Drag On)] ++
            if model.dragOn
                then [ HE.onMouseEnter (SelectRow entry.index)
                     , HE.onMouseLeave (SelectRow entry.index)
                     ]
                else []
        )
        [ H.td
              [ HA.class rowstyle]
              [ H.text <| String.replace "T" " " date ]
        , H.td
            [ ]
            ([ H.input
                  [ HA.class ("pastable " ++ rowstyle)
                  , HA.placeholder "enter your value"
                  , HA.value value
                  , HE.onInput (InputChanged date)
                  , HA.attribute "index" date
                  , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                  ]
                  [ ]
             ] ++ ( if List.member  entry.index  model.lastValids
                        then [ H.button
                                [ HA.title "Fill"
                                , HE.onClick ( FillNas ( entry.index ) )]
                                [ H.text "↓" ]
                        ]
                        else []
                  )
            )
        , H.td
            ([ HA.class "control-col"
               ] ++ if isFirstSelected
                    then [ HA.class "bi bi-clipboard"
                         , HA.class "copy-selection"
                         , HA.title "Copy selection"
                         , HE.onClick CopySelection]
                    else []
                )
            [ ]
        , H.td
            ([ HA.class "control-col"
            ] ++ if isFirstSelected
                    then [ HA.class "remove-selection"
                         , HA.title "Deselect all"
                         , HE.onClick DeselectAll]
                    else []
            )

            [ if isFirstSelected
                then H.text "x"
                else H.text ""
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


debugView: Model -> H.Html Msg
debugView model =
    H.div
        []
        ( if model.horizon.debug
            then
                [( H.text "debug active" )] ++
                [ H.text (", dragMode = " ++ if model.dragOn
                                                    then "On"
                                                    else "Off")]
                  ++ [ H.br [] []]
                  ++ ( List.map
                            (\ i -> H.text (" Na to fill at: " ++ String.fromInt i ))
                            model.lastValids
                     )
            else
                []
        )

displayDate: Maybe String -> List ( H.Html msg )
displayDate date =
    case date of
        Nothing -> []
        Just dat ->
            intercal
                ( H.br [] [] )
                ( List.map
                    (\ part -> H.text part )
                    (String.split "T" dat)
                )
                []


intercal: a -> List a -> List a ->  List a
intercal toAdd parts result =
     case parts of
         [] -> result
         [x] -> result  ++ [x]
         x :: xs -> result ++ [x] ++ [toAdd] ++ intercal toAdd xs result


viewStatTable model =
    H.table

        [ HA.class "stat-table"]
        [ H.th
            [HA.colspan 3]
            [ H.text "Series Meta"]
        , H.tr
            []
            [ H.td
                []
                [H.text "Tzaware"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [H.text ( M.dget "tzaware" model.meta )]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Status"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [H.text ( M.dget "supervision_status" model.meta )]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Type"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [H.text ( M.dget "value_type"  model.meta )]
            ]

        , H.tr
            []
            [ H.td
                []
                [H.text "Source"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [H.text model.source ]
            ]
        , H.tr
            []
            [   H.th
                [HA.colspan 3]
                [ H.text "Data Info"]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Freq"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [ H.text <| Maybe.withDefault
                                ""
                                ( medianValue ( Dict.keys ( getActiveTs model )))
                ]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Start"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                ( displayDate model.statistics.start )

            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "End"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                 ( displayDate model.statistics.end )
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Min"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [ H.text <| case model.statistics.min of
                                Nothing -> ""
                                Just val -> Round.round
                                                model.roundStat
                                                val
                ]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Max"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [ H.text <| case model.statistics.max of
                                Nothing -> ""
                                Just val -> Round.round
                                                model.roundStat
                                                val
                ]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Mean"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [ H.text <| case model.statistics.mean of
                                Nothing -> ""
                                Just val -> Round.round
                                                model.roundStat
                                                val
                ]
            ]
        , H.tr
            []
            [ H.td
                []
                [H.text "Median"]
            , H.td
                []
                [H.text ":"]
            , H.td
                []
                [ H.text <| case model.statistics.median of
                                Nothing -> ""
                                Just val -> Round.round
                                                model.roundStat
                                                val
                ]
            ]
        ]

view : Model -> H.Html Msg
view model =
    let
        maybeMedian = Nothing
        ( dates, values ) = getTs model
        dragMode =
            if model.panActive
            then "pan"
            else "zoom"
        diff = currentDiff model
    in
    H.div
        [HA.class "tseditor"]
        [
    H.div
        [ HA.class "plot-and-stuffs"
        , HE.onMouseUp (Drag Off)]
        [ H.span [ HA.class "action-container" ]
              <| I.viewactionwidgets
                    model
                    convertMsg
                    False
                    "Series Editor"
                    ( getFromToDates model.horizon )
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
        , H.div
            [ ]
            [ H.div
                [ HA.id "plot" ]
                [ ]
            , H.node "plot-figure"
                [ HA.attribute
                    "args"
                    ( serializedPlotArgs
                         "plot"
                        [ scatterplot
                            model.name
                            dates
                            values
                            ( if model.horizon.inferredFreq
                                then "lines+markers"
                                else "lines" )
                            defaultTraceOptions
                        , scatterplot
                            "edition"
                            ( Dict.keys diff )
                            ( diffToFloat ( Dict.values diff ))
                            "lines+markers"
                            defaultTraceOptions
                        ]
                        { defaultLayoutOptions | dragMode = Just dragMode }
                        defaultConfigOptions
                    )
                ]
                [ ]
            ]

        , debugView model
        , permaLink model
        , viewRelevantTable model
        , H.div [] ( List.map (\ err -> H.p [] [H.text err]) model.errors)
        ]
        ,
        H.div
            [ HA.class "stat-position"]
            [ H.div
                [ HA.class "stat-table-container"]
                [ viewStatTable model ]
            ]
    ]



type alias Input =
    { baseurl : String
    , name : String
    , min: String
    , max: String
    , debug: String
    }


init : Input -> ( Model, Cmd Msg )
init input =
     ({ baseurl = input.baseurl
                    , errors = [ ]
                    , name = input.name
                    , meta = Dict.empty
                    , source = ""
                    , seriestype = I.Primary
                    , horizon = initHorizon
                                    input.baseurl
                                    input.min
                                    input.max
                                    input.debug
                                    Loading
                    , initialCommands = Cmd.none
                    , forceDraw = False
                    , intercept = Nothing
                    , slope = Nothing
                    , insertion_dates = Array.empty
                    , processedPasted = [ ]
                    , monotonicCount = 0
                    , rawPasted = ""
                    , firstSelected = Nothing
                    , dragOn = False
                    , lastValids = []
                    , initialTs = Dict.empty
                    , zoomedTs = Nothing
                    , statistics = emptyStat
                    , roundStat = 2
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
                [ zoomPlot FromZoom
                , panActive NewDragMode
                , copySignal CopyFromBrowser
                , onKeyDown keyDecoder
                , loadFromLocalStorage
                    (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
                ]
        }

