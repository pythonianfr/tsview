port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Dateinterval exposing (medianValue)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
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
import Statistics
import Url.Builder as UB
import Util as U
import Json.Encode as JE
import Task as T

port copyToClipboard : String -> Cmd msg
port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg
port panActive : (Bool -> msg) -> Sub msg
port copySignal: (Bool -> msg) -> Sub msg
port saveLocal : LocalStorage -> Cmd msg
port loadLocal : (String -> msg) -> Sub msg


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
    , roundValues: Maybe Int
    -- technical
    , errors : List String
    , rawPasted: String
    , processedPasted: List String
    , initialCommands : Cmd Msg
    , monotonicCount : Int
    , clipboardclass : String
    , panActive : Bool
    -- user actions
    , forceDraw : Bool
    , allowInferFreq : Bool
    -- cells interactivity
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
    | AllowInferFreq
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
    | FromLocal String
    | NewRound ActionRound
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
    { start : TypeStat
    , end: TypeStat
    , min: TypeStat
    , max: TypeStat
    , sum: TypeStat
    , count: TypeStat
    , nas: TypeStat
    , mean: TypeStat
    , p25: TypeStat
    , median: TypeStat
    , p75: TypeStat
    , inferFreq : TypeStat
    }

emptyStat =
    { start = Date Nothing
    , end = Date Nothing
    , min = Numeric Nothing
    , max = Numeric Nothing
    , sum = Numeric Nothing
    , count = Count 0
    , nas = Count 0
    , mean = Numeric Nothing
    , p25 = Numeric Nothing
    , median = Numeric Nothing
    , p75 = Numeric Nothing
    , inferFreq = InferFreq ( Authorised Nothing )
    }

type ActionRound =
    Replace String
    | Remove
    | More
    | Less

type alias LocalStorage =
    { round : Maybe String
    }

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
    , raw: Maybe String
    , index : Int
    , selected: Bool
    }

emptyEntry = Entry
                Nothing
                False
                NoEdition
                Nothing
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
    JD.map6 Entry
        (JD.field "series" (JD.maybe JD.float))
        (JD.field "markers" JD.bool)
        (JD.succeed NoEdition)
        (JD.succeed Nothing)
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

localDecoder : JD.Decoder LocalStorage
localDecoder =
    JD.map LocalStorage
        (JD.field "round" (JD.nullable JD.string))


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
                        statistics = getStatistics
                                        model.allowInferFreq
                                        ( onlyValues indexedval )
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
                                    , statistics = getStatistics
                                                    model.allowInferFreq
                                                    val
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
                default = ( { model | horizon = newModelHorizon}, commands )
                resetModel = { model | horizon = newModelHorizon
                                     , monotonicCount = model.monotonicCount + 1
                                     , zoomedTs = Nothing
                                     , zoomedFormula = Nothing}
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ -> ( { resetModel | forceDraw = False
                                                      , firstSelected = Nothing
                                           }
                                         , commands )
                ModuleHorizon.FromLocalStorage _ ->
                    -- we want to fire the commands AFTER getting the metadata:
                    -- we store these commands in the model -.-
                    ( { resetModel | initialCommands = commands }
                    , Cmd.batch ([ M.getsysmetadata
                                        model.baseurl
                                        model.name
                                        GotMetadata
                                        "series"]))
                ModuleHorizon.Fetch _ -> ( resetModel
                                         , Cmd.batch ([ commands ]
                                         ++ getRelevantData resetModel ))

        SwitchForceDraw ->
            ( flipForce model , Cmd.none)

        AllowInferFreq ->
            ({ model | allowInferFreq = True
                     , statistics = getRelevantStatistics
                                        True
                                        ( onlyValues  model.initialTs )
                                        model.initialFormula
             }
            , Cmd.none
            )

        InputChanged date rawvalue ->
            let
                raw = String.replace " " "" rawvalue
                edition = parseInput raw
            in
            ( setupNas
                <| setOnActiveTs model
                        <| patchWithValue
                            (getActiveTs model)
                            ( date, edition )
                            raw
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
                        (\ _ e -> { e | edited = NoEdition
                                      , raw = Nothing })
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


        NewRound action ->
            let save = (\ newRound ->
                            ( { model | roundValues = newRound}
                            , saveLocal { round =
                                            case newRound of
                                                Nothing -> Nothing
                                                Just round -> Just ( String.fromInt
                                                                        round )
                                        }
                            )
                       )
            in
            case action of
                Remove -> save Nothing
                Replace stuff ->
                    if stuff == ""
                        then save Nothing
                        else
                            case String.toInt stuff of
                                Nothing -> U.nocmd model
                                Just val -> save ( Just val )
                More ->
                    case model.roundValues of
                        Nothing -> save ( Just 0 )
                        Just round -> save ( Just ( round + 1 ) )
                Less ->
                    case model.roundValues of
                        Nothing -> U.nocmd model
                        Just round -> save ( Just ( round - 1 ) )


        FromLocal payload ->
             case JD.decodeString localDecoder payload of
                 Err _ -> U.nocmd model
                 Ok local ->
                     case local.round of
                         Nothing -> U.nocmd { model | roundValues = Nothing }
                         Just round -> U.nocmd { model | roundValues = ( String.toInt  round)}


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
                                               , statistics = getRelevantStatistics
                                                                model.allowInferFreq
                                                                ( onlyValues  model.initialTs )
                                                                model.initialFormula
                                        }
                            Just (minDate, maxDate)
                                -> let zoomedTs =  newZoom
                                                        minDate
                                                        maxDate
                                                        model.initialTs
                                                        model.zoomedTs
                                                        model.panActive
                                       formulaTs = newZoom
                                                        minDate
                                                        maxDate
                                                        model.initialFormula
                                                        model.zoomedFormula
                                                        model.panActive
                                  in
                                    { model | zoomedTs = Just zoomedTs
                                            , zoomedFormula = Just formulaTs
                                            , horizon = { horizonmodel | zoomBounds = Just (minDate, maxDate) }
                                            , monotonicCount = model.monotonicCount + 1
                                            , statistics = getRelevantStatistics
                                                                model.allowInferFreq
                                                                ( onlyValues zoomedTs )
                                                                 formulaTs

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


patchWithValue: Dict String Entry -> (String , Edited) -> String -> Dict String Entry
patchWithValue series (date, edition) raw =
    let
        newentry =
            updateEntry edition ( Just raw )
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


onlyJustValues: Dict String ( Maybe Float ) -> List Float
onlyJustValues series =
     List.concat
        <| List.map
            (\ (k, v) -> case v of
                            Nothing -> []
                            Just val -> [ val ]
            )
            (Dict.toList series)


onlyValues: Dict String Entry -> Dict String ( Maybe Float )
onlyValues series =
    Dict.map
        (\ k e -> e.value )
        series


getStatistics: Bool -> Dict String ( Maybe Float )-> Statistics
getStatistics allowInfer series =
    let dates = List.sort ( Dict.keys series )
        values = List.sort <| onlyJustValues series
        length = List.length values
    in
        { start = Date <| List.head dates
        , end = Date <| List.last dates
        , min = Numeric <| Stat.minimum values
        , max = Numeric <| Stat.maximum values
        , count = Count length
        , nas = Count <| ( List.length dates ) - length
        , sum = if length == 0
                    then Numeric Nothing
                    else Numeric ( Just ( List.sum values ))
        , mean = Numeric <| Stat.mean values
        , p25 = Numeric <| Statistics.quantile 0.25 values
        , median = Numeric <| Stat.median values
        , p75 = Numeric <| Statistics.quantile 0.75 values
        , inferFreq = InferFreq <| if ( length < maxPoints || allowInfer )
                        then (Authorised ( medianValue ( Dict.keys series)) )
                        else Blocked
        }


getRelevantStatistics: Bool ->Dict String ( Maybe Float ) -> Dict String ( Maybe  Float )
                        -> Statistics
getRelevantStatistics allowInfer series formula =
    if Dict.isEmpty series
        then getStatistics allowInfer formula
        else getStatistics allowInfer series


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
                                { e | edited = Deletion
                                    , raw = Nothing
                                }
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


firstSelected: List Entry -> ( List Entry, Maybe Int)
firstSelected entries =
    case entries of
        [] -> ( [], Nothing )
        x::xs ->
            if x.selected
                then ( [], Just x.index )
                else firstSelected xs


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
                                (updateEntry value Nothing)
                                dict
        )
        (\_ _ dict -> dict)
        ( getActiveTs model )
        copyPastedDict
        ( getActiveTs model )


updateEntry : Edited ->  Maybe String -> Maybe Entry -> Maybe Entry
updateEntry value raw maybeEntry  =
    case maybeEntry of
        Nothing -> Nothing
        Just entry ->
             Just { entry | edited = value
                          , raw = raw
                  }


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


underThePlot: Model -> H.Html Msg
underThePlot model =
    H.div
        [ HA.class "under-the-plot" ]
        ( maybeRoundForm model )


permaLink: Model -> H.Html Msg
permaLink model =
    H.a
        [ HA.href ( UB.crossOrigin
                        model.baseurl
                        ["tseditor"]
                        ( queryNav model model.name ))
        , HA.target "_blank"
        , HA.class "permalink"
        ]
        [ H.text "Permalink"]


maybeRoundForm: Model -> List ( H.Html Msg )
maybeRoundForm model =
    case model.seriestype of
        I.Primary ->  [ H.div
                        [ HA.class "form-round"]
                        [ permaLink model]
                      ]
        I.Formula ->
            [ H.div
                [ HA.class "form-round"]
                [ permaLink model
                , H.text "Decimals : "
                 ,H.button
                    [ HA.class "increment-round"
                    , HE.onClick ( NewRound Less )]
                    [ H.text "-" ]
                , H.input
                    [ HA.class "round-input"
                    , HE.onInput (\ s -> NewRound (Replace s) )
                    , HA.value ( printRound model.roundValues )
                    ]
                    []
                , H.button
                    [ HA.class "increment-round"
                    , HE.onClick ( NewRound More )]
                    [ H.text "+" ]
                , H.button
                    [ HA.class "remove-round"
                    , HE.onClick ( NewRound Remove )]
                    [ H.text "X" ]
                ]
                ]


printRound: Maybe Int -> String
printRound stuff =
    case stuff of
        Nothing -> ""
        Just something ->
            String.fromInt something


printStatus plotstatus =
    case plotstatus of
        Loading ->
            "Saving ... please wait"
        Success ->
            "Save"
        _ ->
            "Saving Impossible"

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


nodeTriggerPastable: Model -> H.Html Msg
nodeTriggerPastable model =
    H.node "eval-js"
    [ HA.attribute
          "myjs"
          ( "applyCopyPaste("
             ++ String.fromInt model.monotonicCount
             ++ ");")
    ]
    [ ]


buttonFillAll: Model -> List (H.Html Msg)
buttonFillAll model =
    if model.lastValids /= []
        then [ H.button
                    [ HA.class "bluebutton"
                    , HE.onClick FillAll
                    ]
                    [ H.text "Fill all Nas ↓" ]
             ]
        else
            [ H.div [] [] ]


viewRelevantTable: Model -> H.Html Msg
viewRelevantTable model =
    if model.seriestype == I.Primary
        then viewEditTable model
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
            , H.td [] [ H.text ( printValue model.roundValues
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


buildFormater: Maybe Int -> String
buildFormater maybeRounds =
    case maybeRounds of
        Nothing -> "n"
        Just round -> ",." ++ String.fromInt round ++ "f"


addComponentCells: Model -> String -> List (H.Html Msg)
addComponentCells model date =
    List.map
        ( \ comp -> H.td [] [H.text
                            <| printValue model.roundValues
                               <| Maybe.withDefault
                                   Nothing
                                   <| Dict.get
                                        date
                                        <| Maybe.withDefault
                                            Dict.empty
                                              <|Dict.get
                                                    comp.name
                                                    model.componentsData ])
        model.components


printValue: Maybe Int -> Maybe Float -> String
printValue round value =
    case value of
        Nothing -> ""
        Just val -> formatNumber
                        <| roundNumber
                            round
                            val


roundNumber: Maybe Int -> Float -> String
roundNumber round number =
    case round of
        Nothing -> String.fromFloat number
        Just r -> Round.round r number


formatNumber: String -> String
formatNumber number =
    let parts = String.split "." number
    in
      case parts of
          [] -> ""
          [x] ->  String.reverse
                    <| addSpace
                        <| String.reverse x
          x :: xs ->
            String.concat
                [ String.reverse
                    <| addSpace
                        <| String.reverse x
                , "."
                , String.concat xs
                ]


addSpace: String -> String
addSpace part =
   String.fromList
        <|addSpaceRec
            (String.toList part)
            3
            []


addSpaceRec: List Char -> Int -> List Char -> List Char
addSpaceRec parts counter result =
    if counter == 0 && not (List.isEmpty parts)
        then addSpaceRec
                parts
                3
                ( List.append result [' '] )
        else
            case parts of
                [] -> result
                x :: xs ->
                    addSpaceRec
                        xs ( counter - 1 )
                        ( List.append result [ x ])

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


viewEditTable : Model -> H.Html Msg
viewEditTable model =
    let
        patch = currentDiff model
    in
    H.div
        [ HA.class "tables-edition" ]
        [ commonHeaderEdition model patch
        , editTable model
        , divSaveDataTable patch
        ]


commonHeaderEdition : Model -> Dict String Entry -> H.Html Msg
commonHeaderEdition model patch =
    H.div
        [ HA.class "header-tables-edition" ]
        ( buttonFillAll model ++
          [ H.div
              [ HA.class "for-current-diff"]
              ( divLinearCorrection model patch ++
                saveButtons model patch )
          ]
        )


saveButtons: Model -> Dict String Entry -> List (H.Html Msg)
saveButtons model patch =
    if Dict.isEmpty patch
        then [ ]
        else
            [ H.div
                [ HA.class "save-buttons" ]
                [ H.button
                  [ HA.class "bluebutton"
                  , HA.attribute "type" "button"
                  , HE.onClick CancelEdition
                  , HA.disabled (model.horizon.plotStatus == Loading)
                  ]
                  [ H.text "Cancel" ]
                , H.button
                  [ HA.class "greenbutton"
                  , HA.attribute "type" "button"
                  , HE.onClick SaveEditedData
                  , HA.disabled (model.horizon.plotStatus == Loading)
                  ]
                  [ H.text ( printStatus model.horizon.plotStatus ) ]
                ]
            ]


type StatePoints =
    NoPoint
    | TooMuchPoints Int
    | Drawable


statePoints : Int -> Bool -> StatePoints
statePoints nbPoints forceDraw=
    if nbPoints == 0
        then NoPoint
        else
            if ( nbPoints < maxPoints ) || forceDraw
                then Drawable
                else TooMuchPoints nbPoints


editTable : Model -> H.Html Msg
editTable model =
    let
        class = HA.class "data-table"
    in
    case statePoints
            (Dict.size ( getActiveTs model ))
            model.forceDraw
    of
        NoPoint
            ->  H.div [ class ][ ]
        TooMuchPoints nbPoints ->
            H.div
                [ class ]
                [ msgTooManyPointsWithButton nbPoints
                -- for some reason an existing input help
                -- to apply the method from a blank page
                , H.input
                    [ HA.class "pastable"
                    , HA.hidden True]
                    []
                , nodeTriggerPastable model
                ]
        Drawable ->
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
                , nodeTriggerPastable model
                ]


divSaveDataTable : Dict String Entry -> H.Html Msg
divSaveDataTable filtredDict =
    if Dict.isEmpty filtredDict then
        H.div [] []
    else
        H.div
            [HA.class "save-data-table"]
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
                      (List.map rowSave (Dict.toList filtredDict))
                  ]
            ]

rowSave : (String, Entry) -> H.Html Msg
rowSave (date, entry) =
    H.tr
        [ ]
        [ H.td [ ] [ H.text date ]
        , H.td [ ] [ H.text (case entry.edited of
                                Edition val -> String.fromFloat val
                                _ -> ""
                            )
                    ]
        ]

divLinearCorrection: Model -> Dict String Entry -> List (H.Html Msg)
divLinearCorrection model filtredDict =
    case ( Dict.isEmpty filtredDict ) of
        False -> [
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
            ]
        True ->
            [ ]



getValue: Entry -> String
getValue entry =
    case entry.raw of
        Nothing->
            case entry.edited of
                Edition v -> String.fromFloat v
                Error s -> s
                Deletion -> ""
                NoEdition ->
                    Maybe.unwrap
                        ""
                        String.fromFloat
                        entry.value
        Just stuff -> stuff

rowStyle: Entry -> String
rowStyle entry =
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


viewrow : Model -> ( String, Entry ) -> H.Html Msg
viewrow model ( date, entry ) =
    let
        rowstyle = rowStyle entry
        isFirstSelected = case model.firstSelected of
                            Nothing -> False
                            Just first ->
                                if entry.index == first
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
        ([ H.td
              [ HA.class rowstyle]
              [ H.text <| String.replace "T" " " date ]
         , H.td
            [ ]
            ([ H.input
                  [ HA.class ("pastable " ++ rowstyle)
                  , HA.placeholder "enter your value"
                  , HA.value ( formatNumber ( getValue entry ))
                  , HE.onInput (InputChanged date)
                  , HA.attribute "index" date
                  , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                  ]
                  [ ]
             ] ++ ( case List.member  entry.index  model.lastValids of
                        True -> [ H.button
                                    [ HA.title "Fill"
                                    , HE.onClick ( FillNas ( entry.index ) )]
                                    [ H.text "↓" ]
                                ]
                        False -> []
                  )
            )
         ] ++ (buttonsFirstSelected isFirstSelected )
        )


buttonsFirstSelected: Bool -> List (H.Html Msg)
buttonsFirstSelected predicat =
    if not predicat
        then []
        else [
            H.td
                [ HA.class "control-col"
                , HA.class "bi bi-clipboard"
                , HA.class "copy-selection"
                , HA.title "Copy selection"
                , HE.onClick CopySelection
                ]
                [ ]
            , H.td
                [ HA.class "control-col"
                , HA.class "remove-selection"
                , HA.title "Deselect all"
                , HE.onClick DeselectAll
                ]
                [ H.text "x"]
            ]


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


type TypeStat =
    Numeric ( Maybe Float )
    | Count Int
    | Date ( Maybe String )
    | InferFreq Freq


type Freq =
    Blocked
    | Authorised ( Maybe String )

rowStat : Int -> String -> TypeStat -> H.Html Msg
rowStat round name statistic  =
    let content = case statistic of
                    Numeric num ->
                        case num of
                            Nothing -> [H.text ""]
                            Just value -> [ H.text
                                                <| formatNumber
                                                    <| Round.round
                                                        round
                                                        value
                                          ]
                    Count nb -> [ H.text
                                    <| formatNumber
                                        ( String.fromInt nb )
                                ]
                    Date date -> displayDate date
                    InferFreq infer -> case infer of
                                        Blocked -> [ H.button
                                                    [ HA.class "badge badge-primary h4"
                                                    , HA.title "! Might be costly !"
                                                    , HE.onClick AllowInferFreq ]
                                                    [ H.text "Unlock" ]
                                                    ]
                                        Authorised freq ->
                                            case freq of
                                                Nothing -> []
                                                Just f -> [ H.text f ]
    in
        H.tr
            []
            [ H.td
                []
                [H.text name]
            , H.td
                []
                []
            , H.td
                []
                content
            ]


viewStatTable: Model -> H.Html Msg
viewStatTable model =
    let partialRow = ( rowStat model.roundStat )
    in
    H.table
        [ HA.class "stat-table"]
        [ H.th
            [HA.colspan 3]
            [ H.text "Data Info"]
        , partialRow "Start" model.statistics.start
        , partialRow "End" model.statistics.end
        , partialRow "Min" model.statistics.min
        , partialRow "Max" model.statistics.max
        , partialRow "Sum" model.statistics.sum
        , partialRow "Count" model.statistics.count
        , partialRow "NaNs" model.statistics.nas
        , partialRow "Mean" model.statistics.mean
        , partialRow "P25" model.statistics.p25
        , partialRow "P50" model.statistics.median
        , partialRow "P75" model.statistics.p75
        , partialRow "Freq" model.statistics.inferFreq
        ]


displayStatus: Model -> H.Html Msg
displayStatus model =
    if model.horizon.plotStatus == None
      then H.text "The graph is loading, please wait"
      else if isEmpty model && (model.horizon.plotStatus == Success)
           then H.text """It seems there is no data to display in this
                        interval, select another one."""
           else H.text ""


plotNode: Model -> H.Html Msg
plotNode model =
    let ( dates, values ) = getTs model
        diff = currentDiff model
        dragMode =
            if model.panActive
            then "pan"
            else "zoom"
        yaxis = defaultLayoutOptions.yaxis
        newYaxis =  { yaxis | hoverFormat = Just <| buildFormater
                                                        model.roundValues
                    }
    in
    H.node "plot-figure"
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
                    { defaultLayoutOptions | dragMode = Just dragMode
                                           , yaxis = newYaxis
                    }
                    defaultConfigOptions
                )
            ]
            [ ]


view : Model -> H.Html Msg
view model =
    let
        maybeMedian = Nothing
    in
    H.div
        [HA.class "tseditor"
        , HE.onMouseUp (Drag Off)
        ]
        [ H.div
            [ HA.class "header-with-horizon"]
            [ H.div [ HA.class "action-container" ]
                  <| I.viewactionwidgets
                        model
                        convertMsg
                        False
                        "Series Editor"
                        ( getFromToDates model.horizon )
            , I.viewtitle model maybeMedian CopyNameToClipboard
            ]
        , H.div
            [ HA.class "under-the-header"]
            [ H.div
                [ HA.class "plot-and-stuffs" ]
                [ H.div
                    [ HA.class "status-plot" ]
                    [ displayStatus model ]
                , H.div
                    [ HA.id "plot" ]
                    [ ]
                , plotNode model
                , debugView model
                , underThePlot model
                , viewRelevantTable model
                , H.div [] ( List.map (\ err -> H.p [] [H.text err]) model.errors)
                ]
                , H.div
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
                    , allowInferFreq = False
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
                    , roundValues = Nothing
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
                , loadLocal FromLocal
                , loadFromLocalStorage
                    (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
                ]
        }

