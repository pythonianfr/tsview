port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Either exposing (Either(..))
import Horizon exposing
    ( HorizonModel
    , LocalStorageData
    , Offset
    , defaultHorizon
    , horizons
    , loadFromLocalStorage
    , localstoragedecoder
    , saveToLocalStorage
    , updatefromlocalstorage
    , updateHorizon
    , updateHorizonModel
    , updateOffset
    , horizonwidget
    )
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
    ( getdata
    , scatterplot
    , plotargs
    )
import Process as P
import Url.Builder as UB
import Util as U
import Json.Encode as JE
import Random
import Task as T


port copyToClipboard : String -> Cmd msg
port dateInInterval : (List String -> msg) -> Sub msg


type alias Model =
    { baseurl : String
    , errors : List String
    , meta : M.StdMetadata
    , source : String
    , seriestype : I.SeriesType
    , date_index : Int
    , horizon : HorizonModel Entry
    , indexToInsert: Maybe String
    , insertion_dates : Array String
    , name : String
    , processedPasted: List String
    , rawPasted: String
    , initialTs : Dict String Entry
    , view_nocache : Bool
    , randomNumber : Int
    , plotStatus : PlotStatus
    , clipboardclass : String
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotMetadata (Result Http.Error String)
    | HorizonSelected (Maybe String)
    | UpdateOffset Offset
    | InputChanged String String
    | SaveEditedData
    | GotEditedData (Result Http.Error String)
    | Paste PasteType
    | InsertionDates (Result Http.Error String)
    | GetLastInsertionDates (Result Http.Error String)
    | GetLastEditedData (Result Http.Error String)
    | RandomNumber Int
    | TimeZoneSelected String
    | InferredFreq Bool
    | FromLocalStorage String
    | NewDates (List String)
    | CopyNameToClipboard
    | ResetClipboardClass


type alias Entry =
    { value : Maybe Float
    , override : Bool
    , edited : Maybe String
    , index : Int
    }


type alias PasteType =
    { text: String
    , index: String
    }


type PlotStatus
    = Init
    | Loading
    | Success
    | Failure


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


pasteditems : String -> List String
pasteditems raw =
    if String.contains "\n" raw
    then String.split "\n" raw
    else if String.contains "\r" raw
    then String.split "\r" raw
    else if String.contains " " raw
    then String.split " " raw
    else [ raw ]


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


geteditor : Model -> (Result Http.Error String -> Msg) -> Cmd Msg
geteditor model callback =
    getdata
    { baseurl = model.baseurl
    , name = model.name
    , idate = Nothing
    , callback = callback
    , nocache = (U.bool2int model.view_nocache)
    , fromdate =
        Maybe.unwrap "" (always model.horizon.mindate) model.horizon.horizon
    , todate = Maybe.unwrap "" (always model.horizon.maxdate) model.horizon.horizon
    , horizon = model.horizon.horizon |> Maybe.andThen
                (\key-> OD.get key horizons) |> Maybe.map
          (String.replace "{offset}" (String.fromInt model.horizon.offset))
    , tzone = model.horizon.timeZone
    , inferredFreq = model.horizon.inferredFreq
    , keepnans = True
    , apipoint = "supervision"
    }


reindex : Int -> ( String, Entry ) -> ( String, Entry )
reindex increment keyvalue =
    let
        data = Tuple.second keyvalue
    in
    ( Tuple.first keyvalue
    , { data | index = increment }
    )


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
                    U.nocmd { model
                                | plotStatus = Success
                                , initialTs = indexedval
                                , horizon = updateHorizonModel model.horizon indexedval
                            }
                Err err ->
                  doerr "series decode" <| JD.errorToString err

        GotEditData (Err _) ->
            U.nocmd { model | plotStatus = Failure }

        HorizonSelected horizon ->
            let
                userprefs =
                    LocalStorageData
                        horizon
                        model.horizon.timeZone
                        model.horizon.inferredFreq

                newmodel =
                    { model
                        | plotStatus = Loading
                        , horizon = updateHorizon horizon model.horizon
                    }
            in
            ( newmodel
            , Cmd.batch
                [ geteditor newmodel GotEditData
                , Random.generate RandomNumber randomInt
                , saveToLocalStorage userprefs
                ]
            )

        UpdateOffset (Left i) ->
            updateModelOffset model i

        UpdateOffset (Right i) ->
            updateModelOffset model -i

        InputChanged date value ->
            let
                newtimeSeries =
                    Dict.update
                        date (updateEntry (parseCopyPastedData value)) model.horizon.timeSeries
                newHorizonModel =
                    model.horizon
            in
            U.nocmd { model
                        | horizon = { newHorizonModel | timeSeries = newtimeSeries }
                    }

        GetLastInsertionDates (Ok rawdates) ->
            case JD.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    if (Array.length model.insertion_dates) /= (List.length dates) then
                        ( model, geteditor model GetLastEditedData )
                    else
                        ( model, patchEditedData model )

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
                            Dict.union model.horizon.timeSeries indexedval

                        newmodel =
                            { model
                                | horizon = updateHorizonModel model.horizon patched
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
            ( { model | plotStatus = Loading }
            , I.getidates model "series" GetLastInsertionDates
            )

        GotEditedData (Ok _) ->
            ( model
            , Cmd.batch
                  [ geteditor model GotEditData
                  , Random.generate RandomNumber randomInt
                  ]
            )

        GotEditedData (Err _) ->
            U.nocmd { model | plotStatus = Failure }

        GotMetadata (Ok result) ->
            case JD.decodeString M.decodemeta result of
                Ok allmeta ->
                    U.nocmd { model | meta = allmeta }
                Err err ->
                    doerr "gotmeta decode" <| JD.errorToString err

        GotMetadata (Err err) ->
            doerr "gotmeta http" <| U.unwraperror err

        Paste payload ->
            let
                newtimeSeries = getPastedDict model payload
                newHorizonModel = model.horizon
            in
            U.nocmd { model | horizon = { newHorizonModel | timeSeries = newtimeSeries } }

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

        RandomNumber number ->
            U.nocmd { model | randomNumber = number }

        TimeZoneSelected timeZone ->
            let
                newHorizonModel = model.horizon
                newModel = { model
                               | plotStatus = Loading
                               , horizon = { newHorizonModel | timeZone = timeZone }
                           }
                userprefs =
                    LocalStorageData
                        model.horizon.horizon
                        timeZone
                        model.horizon.inferredFreq

            in
            ( newModel
            , Cmd.batch
                [ geteditor newModel GotEditData
                , I.getidates newModel "series" InsertionDates
                , saveToLocalStorage userprefs
                ]
            )

        InferredFreq isChecked ->
            let
                newHorizonModel = model.horizon
                newModel = { model
                               | plotStatus = Loading
                               , horizon = { newHorizonModel | inferredFreq = isChecked }
                           }
                userprefs =
                    LocalStorageData
                        model.horizon.horizon
                        model.horizon.timeZone
                        isChecked
            in
            ( newModel
            , Cmd.batch
                [ geteditor newModel GotEditData
                , saveToLocalStorage userprefs
                ]
            )

        FromLocalStorage rawdata ->
            case JD.decodeString localstoragedecoder rawdata of
                Ok datadict ->
                    let
                        newmodel =
                            { model
                                | plotStatus = Loading
                                , horizon = updatefromlocalstorage datadict model.horizon
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                        [ geteditor newmodel GotEditData
                        , Random.generate RandomNumber randomInt
                        ]
                    )
                Err _ ->
                    ( model
                    , geteditor model GotEditData
                    )

        NewDates dates ->
            let
                horizonmodel =
                    model.horizon
                newmodel =
                    if List.isEmpty dates then
                        { model | horizon = { horizonmodel | timeSeries = model.initialTs } }
                    else
                        { model
                            | horizon =
                              { horizonmodel |
                                    timeSeries = Dict.filter
                                                 (\key _ -> List.member key dates)
                                                 horizonmodel.timeSeries
                              }
                        }
            in
            ( newmodel
            , Random.generate RandomNumber randomInt
            )

        CopyNameToClipboard ->
            ( { model | clipboardclass = "bi bi-check2" }
            , Cmd.batch
                [ copyToClipboard model.name
                , T.perform (always (ResetClipboardClass)) (P.sleep 1000)
                ]
            )

        ResetClipboardClass ->
            U.nocmd { model | clipboardclass = "bi bi-clipboard" }



randomInt : Random.Generator Int
randomInt =
  Random.int 0 999999999


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
                (Dict.get (payload.index) model.horizon.timeSeries)
        listIndex =
            List.range firstIndex (firstIndex + (List.length newValues) - 1)
        listDates =
            Dict.keys
                (Dict.filter
                     (\_ value -> List.member value.index listIndex)
                     model.horizon.timeSeries
                )
        copyPastedDict =
            Dict.fromList <| List.map2 Tuple.pair listDates newValues
    in
    Dict.merge
        (\_ _ dict -> dict)
        (\key _ value dict -> Dict.update key (updateEntry value) dict)
        (\_ _ dict -> dict)
        model.horizon.timeSeries
        copyPastedDict
        model.horizon.timeSeries


updateEntry : Maybe String -> Maybe Entry -> Maybe Entry
updateEntry value maybeEntry =
    maybeEntry
        |> Maybe.andThen
           (\entry ->
                if (parseCopyPastedData (Maybe.unwrap "" String.fromFloat entry.value)) /= value
                then Just { entry | edited = value }
                else Just { entry | edited = Nothing }
           )


updateModelOffset : Model -> Int -> (Model, Cmd Msg)
updateModelOffset model i =
    let
        offset = (model.horizon.offset + i)
        newModel =
            { model | plotStatus = Loading
            , horizon = updateOffset offset model.horizon
            }
    in
    ( newModel
    , Cmd.batch
        [ geteditor newModel GotEditData
        , Random.generate RandomNumber randomInt
        ]
    )


patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let
        tzaware =
            case Dict.get "tzaware" model.meta of
                Just (M.MBool val) -> val
                _ -> False

        patch =
            model.horizon.timeSeries
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


divButtonSaveData : PlotStatus -> Dict String Entry -> H.Html Msg
divButtonSaveData plotStatus filtredDict =
    let
        className = [ HA.class "button-save-data" ]
        textButton =
            if plotStatus == Loading
            then "Saving ... please wait"
            else if plotStatus == Success
                 then "Save"
                 else "Fail, please try again"
    in
    if Dict.isEmpty filtredDict then
        H.div
            className
            [ ]
    else
        H.div
            className
            [ H.button
                  [ HA.class "greenbutton"
                  , HA.attribute "type" "button"
                  , HE.onClick SaveEditedData
                  , HA.disabled (plotStatus == Loading)
                  ]
                  [ H.text textButton ]
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
                  ("applyCopyPaste(" ++ String.fromInt model.randomNumber ++ ");")
            ]
            [ ]
        class = HA.class "data-table"
    in
    if Dict.isEmpty model.horizon.timeSeries
    then H.div [ class ][ ]
    else
        if Dict.size model.horizon.timeSeries > 1000
        then H.div
            [ class ]
            [ H.text """ Too many points to display. Please select a smaller time
                      frame or an area on the graph."""
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
                      (List.map viewRow (Dict.toList model.horizon.timeSeries))
                  ]
            , node
            ]


divTablesSection : Model -> H.Html Msg
divTablesSection model =
    let
        filtredDict = Dict.filter
            (\_ entry -> Maybe.isJust entry.edited)
            model.horizon.timeSeries
    in
    H.div
        [ HA.class "tables" ]
        [ divButtonSaveData model.plotStatus filtredDict
        , editTable model
        , divSaveDataTable filtredDict
        ]


viewRow : (String, Entry) -> H.Html Msg
viewRow ( date, entry ) =
    let
        data =
            if Maybe.isJust entry.edited
            then Maybe.withDefault "" entry.edited
            else Maybe.unwrap "" String.fromFloat entry.value

        rowStyle =
            if Maybe.isJust entry.edited
            then "row-green"
            else if entry.override
                 then "row-blue"
                 else if Maybe.isNothing entry.value
                      then "row-red"
                      else ""
    in
    H.tr [ ]
        [ H.td
              [ HA.class rowStyle]
              [ H.text <| String.replace "T" " " date ]
        , H.td
            [ ]
            [ H.input
                  [ HA.class ("pastable " ++ rowStyle)
                  , HA.placeholder "enter your value"
                  , HA.value data
                  , HE.onInput (InputChanged date)
                  , HA.attribute "index" date
                  , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                  ]
                  [ ]
            ]
        ]


viewPlotData : Model -> H.Html Msg
viewPlotData model =
    let
        plot =
            scatterplot model.name
                (Dict.keys model.horizon.timeSeries)
                (List.map (\x -> x.value) (Dict.values model.horizon.timeSeries))
                "lines"
        args =
            plotargs "plot" [ plot ]
    in
    H.div
        [ HA.class "graph"]
        [ H.div [ HA.id "plot" ] [ ]
        , H.node "plot-figure" [ HA.attribute "args" args ] [ ]
        ]


statusText : PlotStatus -> String
statusText plotStatus =
    if plotStatus == Init then
        "Init"
    else if plotStatus == Loading then
        "Loading ..."
    else if plotStatus == Success then
        ""
    else
        "Failure"


horizonevents =
    { inferredFreqMsg = InferredFreq
    , timeZoneMsg = TimeZoneSelected
    , offsetMsg = UpdateOffset
    , timeDeltaMsg = HorizonSelected
    }


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.style "margin" ".5em" ]
        [ H.span [ HA.class "action-container" ]
              <| I.viewactionwidgets model horizonevents
        , I.viewtitle model CopyNameToClipboard
        , H.div
            [ HA.class "status-plot" ]
            [ if model.plotStatus == Init
              then H.text "The graph is loading, please wait"
              else if (Dict.isEmpty model.horizon.timeSeries) && (model.plotStatus == Success)
                   then H.text """It seems there is no data to display in this
                                interval, select another one."""
                   else H.text (statusText model.plotStatus)
            ]
        , viewPlotData model
        , divTablesSection model
        ]


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input Model Msg
main =
    let
        init input =
            let
                model =
                    { baseurl = input.baseurl
                    , errors = [ ]
                    , meta = Dict.empty
                    , source = "local"
                    , seriestype = I.Primary
                    , date_index = 0
                    , horizon =
                          { offset = 0
                          , horizon = Just defaultHorizon
                          , inferredFreq = False
                          , mindate = ""
                          , maxdate = ""
                          , timeSeries = Dict.empty
                          , timeZone = "UTC"
                          }
                    , indexToInsert = Nothing
                    , insertion_dates = Array.empty
                    , name = input.name
                    , processedPasted = [ ]
                    , randomNumber = 0
                    , rawPasted = ""
                    , initialTs = Dict.empty
                    , view_nocache = False
                    , plotStatus = Init
                    , clipboardclass = "bi bi-clipboard"
                    }
            in
            ( model
            , Cmd.batch
                [ M.getsysmetadata model.baseurl model.name GotMetadata "series"
                , I.getidates model "series" InsertionDates
                ]
            )

    in Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
              \_ -> Sub.batch
                    [ loadFromLocalStorage FromLocalStorage
                    , dateInInterval NewDates
                    ]
    }
