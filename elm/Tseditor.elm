port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events exposing
    ( onKeyDown
    , onKeyUp
    )
import Browser.Navigation
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


keyDecoder : Action -> JD.Decoder Msg
keyDecoder action =
    JD.map ( toKey action )  (JD.field "key" JD.string)


toKey : Action -> String -> Msg
toKey action keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            NoAction
        _ ->
            ActionControl ( keyToType action keyValue )


keyToType:  Action -> String -> ControlKey
keyToType action keyValue =
    if keyValue == "Escape"
    then Escape action
    else
    if keyValue == "Delete"
    then Delete action
    else
    if keyValue == "Control"
    then Control action
    else
    if keyValue == "Shift"
    then Shift action
    else
    if keyValue == "ArrowUp"
    then ArrowUp action
    else
    if keyValue == "ArrowDown"
    then ArrowDown action
    else
    if keyValue == "PageDown"
    then PageDown action
    else
    if keyValue == "PageUp"
    then PageUp action
    else
    if keyValue == "Enter"
    then ArrowDown action
    else Other keyValue action


type Action =
    Up
    | Down


type ControlKey =
    Escape Action
    | Delete Action
    | Control Action
    | Shift Action
    | ArrowUp Action
    | ArrowDown Action
    | PageUp Action
    | PageDown Action
    | Enter Action
    | Other String Action

naiveTag: String
naiveTag = "Naive"

type alias Model =
    { baseurl : String
    , name : String
    , mode: EditionMode
    , meta : M.StdMetadata
    , source : String
    , catalog : Maybe ( List String )
    , seriestype : I.SeriesType
    , horizon : HorizonModel
    , creation: CreationModel
    -- data
    , insertion_dates : Array String
    , series : Series
    , statistics: Statistics
    , roundStat: Int
    , roundValues: Maybe Int
    -- technical
    , errors : List String
    , rawPasted: String
    , processedPasted: List String
    , initialCommands : Cmd Msg
    , monotonicCount : Int
    , statusCopy : StatusCopy
    , panActive : Bool
    -- user actions
    , forceDraw : Bool
    , allowInferFreq : Bool
    -- cells interactivity
    , focus : Maybe Int
    , firstShift: Maybe Int
    , firstSelected : Maybe Int
    , lastValids: List Int
    , slope: Maybe String
    , intercept: Maybe String
    -- keyboard/mouse
    , keyName: String
    , holding: Holding
    -- show-values for formula
    , components : List Component
    , componentsData: Dict String Series
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotValueData (Result Http.Error String)
    | GotComponents (Result Http.Error String)
    | GotComponentData String (Result Http.Error String)
    | GotGenerated (Result Http.Error String)
    | GotMetadata (Result Http.Error String) -- first command fired
    | GotSource (Result Http.Error String)
    | GotCatalog (Result Http.Error String)
    | HasCache ( Result Http.Error String )
    | GotInterval ( Result Http.Error String )
    | Horizon ModuleHorizon.Msg
    | Create CreationOptions
    | SwitchForceDraw
    | AllowInferFreq
    | InputChanged String String
    | SaveEditedData
    | Saved (Result Http.Error String)
    | CancelEdition
    | Correction Parameter
    | Paste PasteType
    | SelectRow Int
    | DeselectAll Bool
    | ClickCell Int
    | Drag DragMode
    | CopySelection
    | CopyFromBrowser Bool
    | CopyToClipboard CopyType
    | ResetClass CopyType
    | ActionControl ControlKey
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



type alias SeriesNaked = Dict String (Maybe Float)
type alias SeriesToEdit = Dict String Entry

type Series =
    Naked { initialTs: SeriesNaked, zoomTs: Maybe SeriesNaked }
    | ToEdit { initialTs: SeriesToEdit, zoomTs: Maybe SeriesToEdit }

emptySeries: Series
emptySeries = Naked { initialTs = Dict.empty, zoomTs = Nothing }


type EditionMode =
    Creation CreationMode
    | Existing I.SeriesType

type CreationMode =
    Form
    | Edit

type CreationOptions =
    Name String
    | From String
    | To String
    | FreqOffset String
    | FreqMultiply String
    | Tz String
    | Value String
    | Preview


type alias FreqType =
    { offset: String
    , multiplier: Maybe Int
    }


type alias CreationModel =
    { from: String
    , to: String
    , freq: FreqType
    , tz: TzSelector
    , value: Maybe Float
    , name: String
    , nameStatus: NameStatus
    , mandatoryValid: Bool
    }

type TzSelector =
    Unchanged
    | Naive
    | Selected String

type NameStatus =
    Valid
    | Invalid
    | Missing

initCreationModel: CreationModel
initCreationModel =
    { from = ""
    , to = ""
    , freq = { offset = "D"
             , multiplier = Nothing }
    , tz = Unchanged
    , value = Nothing
    , name = ""
    , nameStatus = Missing
    , mandatoryValid = False
    }

convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


type Parameter =
    Slope String
    | Intercept String


type alias Statistics =
    { first: TypeStat
    , last: TypeStat
    , start : TypeStat
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
    { first = Date Nothing
    , last = Date Nothing
    , start = Date Nothing
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

type alias Interval =
    { from: String
    , to: String
    }

type alias Holding =
    { mouse: Bool
    , control: Bool
    , shift : Bool
    }

emptyHolding =
    { mouse = False
    , control = False
    , shift = False
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


type CopyType =
    CopyName
    | CopyDates
    | CopyValues

type alias StatusCopy =
    { name : Bool
    , dates: Bool
    , values : Bool
    }


initialStatusCopy: StatusCopy
initialStatusCopy =
    { name = True
    , dates = True
    , values = True
    }

classClip = "bi bi-clipboard"
classCheck = "bi bi-check2"

getCopyClass: StatusCopy -> CopyType -> String
getCopyClass statusCopy copyType =
    case copyType of
        CopyName -> if statusCopy.name
                    then classClip
                    else classCheck
        CopyDates -> if statusCopy.dates
                    then classClip
                    else classCheck
        CopyValues -> if statusCopy.values
                    then classClip
                    else classCheck


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


asEdited: Maybe Float -> Edited
asEdited value =
    case value of
        Nothing -> Deletion
        Just v -> Edition v


dressSeries: SeriesNaked -> Dict String Entry
dressSeries series =
    Dict.fromList
        <| List.indexedMap
            (\ idx (k, v) -> ( k
                             , { value = v
                               , override = False
                               , edited = asEdited v
                               , raw = case v of
                                        Nothing -> Nothing
                                        Just val -> Just
                                                      <| String.fromFloat val
                               , index = idx
                               , selected = False
                               }
                             )
            )
            ( Dict.toList series )


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
    On Int
    | Off

--type alias Series = Dict String (Maybe Float)


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


onlyNames: Dict String ( List ( List String )) -> List String
onlyNames catalog =
    List.map
        (\ l -> Maybe.withDefault "" ( List.head l ))
        <| Maybe.withDefault
            []
            ( List.head ( Dict.values catalog ))


catalogDecoder : JD.Decoder ( List String )
catalogDecoder =
    JD.map
        onlyNames
        <| JD.dict
            ( JD.list (JD.list JD.string ))


getPoints: Model -> Cmd Msg
getPoints model =
    case model.mode of
        Existing I.Primary -> getSeries model GotEditData "supervision" GET model.name
        Existing I.Formula ->  getSeries model GotValueData "state" GET model.name
        Creation _ -> Cmd.none


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


printFreq: FreqType -> String
printFreq freq =
    let offset = freq.offset
    in
    case freq.multiplier of
        Nothing -> offset
        Just nb -> String.fromInt nb ++ offset


getGeneratedTs: Model -> Cmd Msg
getGeneratedTs model =
    Http.get
    { url = (UB.crossOrigin model.baseurl
                [ "generate-ts" ]
                (( [ UB.string "from" model.creation.from
                  , UB.string "to" model.creation.to
                  , UB.string "freq" ( printFreq model.creation.freq )
                  ] ++ case model.creation.tz of
                       Naive -> []
                       Selected tz -> [ UB.string "tz" tz ]
                       Unchanged -> [ UB.string "tz" model.horizon.timeZone ]
                )
                   ++ case model.creation.value of
                       Nothing -> []
                       Just value ->
                        [ UB.string "value" ( String.fromFloat value )]
                )
    )
    , expect = Http.expectString GotGenerated }



getsource : String -> String -> Cmd Msg
getsource baseurl name =
    Http.get
        { expect = Http.expectString GotSource
        , url = UB.crossOrigin baseurl
              [ "api", "series", "source" ]
              [ UB.string "name" name ]
        }

getCatalog: Model -> Cmd Msg
getCatalog model =
    Http.get
        { expect = Http.expectString GotCatalog
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "catalog" ]
              [ UB.string "allsources" "false" ]
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
                            cropTs
                                ( Dict.fromList
                                    <| List.indexedMap
                                            reindex
                                            (Dict.toList val)
                                )
                                ( getFetchBounds model.horizon )

                        zoomTs = case model.horizon.zoomBounds of
                                    Nothing -> Nothing
                                    Just ( min, max ) ->
                                        Just
                                            <| Dict.filter
                                                    (( \k _ -> (( k >= min ) && ( k <= max ))))
                                                    indexedval
                        series = ToEdit { initialTs = indexedval, zoomTs = zoomTs }
                        statistics = getStatistics
                                        model.statistics
                                        model.allowInferFreq
                                        ( onlyActiveValues series )
                    in
                        applyFocus
                        ( setupNas
                            ({ model
                                    | series = series
                                    , statistics = statistics
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    indexedval
                            })
                        )
                        ( Just 0 )

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
                Ok val -> let ts = cropTs val ( getFetchBounds model.horizon )
                          in
                          ( { model | series = Naked { initialTs = ts
                                                     , zoomTs = Nothing }
                                    , horizon = updateHorizonFromData
                                                model.horizon
                                                ts
                                    , monotonicCount = model.monotonicCount + 1
                                    , statistics = getStatistics
                                                model.statistics
                                                model.allowInferFreq
                                                ts
                          }
                           , getComponents model )
                Err err -> U.nocmd ( addError
                                        { model | horizon = setStatusPlot
                                                                model.horizon
                                                                Failure
                                        }
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
                Ok val ->  let newCD = Dict.insert
                                        name
                                        ( Naked { initialTs = val, zoomTs = Nothing} )
                                        model.componentsData
                           in
                               U.nocmd { model | componentsData = newCD }
                Err err -> U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}

        GotComponentData name (Err _) -> U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        GotGenerated ( Ok rawdata ) ->
             case JD.decodeString
                    (JD.dict (JD.maybe JD.float))
                    rawdata of
                Ok val ->  U.nocmd
                            { model | series =
                                        ToEdit { initialTs = dressSeries val
                                               , zoomTs = Nothing
                                               }
                                    , mode = Creation Edit
                            }
                Err err -> U.nocmd
                            { model | errors = model.errors ++ [JD.errorToString err]}

        GotGenerated (Err err) -> U.nocmd { model | horizon = setStatusPlot model.horizon Failure }


        Horizon hMsg ->
            let ( newModelHorizon, commands ) =  updateHorizon
                                                    hMsg
                                                    convertMsg
                                                    model.horizon
                moreCommands = Cmd.batch [ deselect True , commands]
                ( focusM, focusCmd ) = applyFocus { model | horizon = newModelHorizon} Nothing
                default = ( focusM, Cmd.batch [focusCmd, moreCommands ] )
                resetModel = { model | horizon = newModelHorizon
                                     , monotonicCount = model.monotonicCount + 1
                                     , series = case  model.series of
                                                    Naked series -> Naked { initialTs = series.initialTs
                                                                          , zoomTs = Nothing}
                                                    ToEdit series -> ToEdit { initialTs = series.initialTs
                                                                            , zoomTs = Nothing}
                             }
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ -> ( { resetModel | forceDraw = False
                                                        , firstSelected = Nothing
                                           }
                                         , moreCommands )
                ModuleHorizon.FromLocalStorage _ ->
                    -- we want to fire the commands AFTER getting the metadata:
                    -- we store these commands in the model -.-
                    ( { resetModel | initialCommands = moreCommands }
                    , commandStart resetModel

                    )
                ModuleHorizon.Fetch _ -> ( resetModel
                                         , Cmd.batch ([ moreCommands ]
                                         ++ getRelevantData resetModel ))


        Create option ->
            let creation = model.creation
                freq = model.creation.freq
                newCreation = case option of
                    From val -> { creation | from = val }
                    To val -> { creation | to = val }
                    FreqMultiply val ->
                        { creation |
                            freq = { freq |
                                        multiplier =
                                            if val == ""
                                                then Nothing
                                                else String.toInt val
                                    }
                        }
                    FreqOffset val ->
                        { creation |
                            freq = { freq | offset = val }
                        }
                    Tz val -> { creation | tz = if val == naiveTag
                                                    then Naive
                                                    else Selected val
                              }
                    Value val -> { creation | value = String.toFloat val }
                    Name val -> { creation | name = val
                                           , nameStatus = case model.catalog of
                                                            Nothing -> Invalid
                                                            Just cat ->
                                                                if List.member val cat
                                                                    then Invalid
                                                                    else
                                                                        if val == ""
                                                                            then Missing
                                                                            else Valid
                                }
                    Preview -> creation
                validatedCreation = { newCreation | mandatoryValid =  newCreation.from /= "" &&
                                                                      newCreation.to /= ""
                                    }

                command = case option of
                    Preview -> getGeneratedTs model
                    _ -> Cmd.none
                tzawarness = case validatedCreation.tz of
                                Naive -> False
                                _ -> True
            in
                ( { model | creation = validatedCreation
                          , name = validatedCreation.name
                          , meta = Dict.fromList
                                    [( "tzaware", M.MBool tzawarness )
                                    ,( "tzaware", M.MBool tzawarness )]
                  }
                , command )


        SwitchForceDraw ->
            applyFocus
                ( setupNas ( flipForce model ))
                ( Just 0 )

        AllowInferFreq ->
            ({ model | allowInferFreq = True
                     , statistics = getStatistics
                                        model.statistics
                                        True
                                        ( onlyActiveValues model.series )
             }
            , Cmd.none
            )

        InputChanged date rawvalue ->
            let
                rawstring = String.replace " " "" rawvalue
                edition = parseInput rawstring
                raw = if rawstring == ""
                            then Nothing
                            else Just rawstring
            in
            ( setupNas
                <| setOnEdtitionTs model
                        <| patchWithValue
                            ( getEditionTs model.series )
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
                            Dict.union
                                ( getEditionTs model.series )
                                indexedval

                        newmodel =
                             { model
                                | horizon = updateHorizonFromData
                                                model.horizon
                                                patched
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
            let command = case model.mode of
                            Existing _ -> Cmd.batch
                                            [ I.getidates model "series" GetLastInsertionDates
                                            , deselect True
                                            ]
                            Creation _ -> Cmd.batch
                                            [ patchEditedData model
                                            , deselect True
                                            ]
            in
            ( { model | horizon = ( setStatusPlot model.horizon Loading )}
            , command
            )

        CancelEdition ->
            let newmodel = { model | slope = Nothing
                                   , intercept = Nothing
                           }
            in
            applyFocus
                ( setupNas
                    (setOnEdtitionTs model
                        (Dict.map
                            (\ _ e -> { e | edited = NoEdition
                                          , raw = Nothing })
                            ( getEditionTs newmodel.series ))
                    )
                )
                ( model.focus )

        Correction param ->
            case param of
                Slope value ->  U.nocmd { model | slope = Just value}
                Intercept value -> U.nocmd { model | intercept = Just value}

        Saved (Ok _) ->
            case model.mode of
                Existing _ ->
                    ( { model | monotonicCount = model.monotonicCount + 1
                              , slope = Nothing
                              , intercept = Nothing
                      }
                    , Cmd.batch
                          ( getRelevantData model)
                    )
                Creation _ -> ( model
                              , Browser.Navigation.load
                                    <| UB.crossOrigin
                                            model.baseurl
                                            [ "tseditor" ]
                                            [ UB.string "name" model.name ]
                              )

        Saved (Err _) ->
            U.nocmd { model | horizon = ( setStatusPlot model.horizon Failure )}

        GotMetadata (Ok result) ->
            case JD.decodeString M.decodemeta result of
                Ok allmeta ->
                   let seriestype = if Dict.member "formula" allmeta
                                                            then I.Formula
                                                            else  I.Primary
                       newmodel = { model | meta = allmeta
                                          , seriestype = seriestype
                                          , mode = case seriestype of
                                              I.Primary -> Existing I.Primary
                                              I.Formula -> Existing I.Formula
                                          , monotonicCount = model.monotonicCount + 1
                                  }
                   in
                       ( newmodel
                       , Cmd.batch ([ getHasCache newmodel
                                    , model.initialCommands
                                    , getInterval model.baseurl model.name
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

        GotCatalog (Ok raw) ->
            case JD.decodeString catalogDecoder raw of
                Ok catalog ->
                    U.nocmd { model | catalog = Just catalog }
                Err err ->
                    doerr "gotcatalog decode" <| JD.errorToString err

        GotCatalog (Err err) ->
            doerr "gotcatalog http" <| U.unwraperror err

        HasCache (Ok rawhascache) ->
            let model_horizon = model.horizon
            in
                U.nocmd { model | horizon =
                            { model_horizon | hasCache = String.startsWith
                                                            "true"
                                                            rawhascache }}

        HasCache (Err error) ->
            doerr "hascache http" <| U.unwraperror error

        GotInterval ( Ok raw ) ->
            let decodeStuff = JD.oneOf
                                [ JD.string
                                , JD.map (\ _ -> "isABool" ) JD.bool ]
            in
            case JD.decodeString ( JD.list decodeStuff ) raw of
                Ok payLoad ->
                    case payLoad of
                        [ _, first, last ] ->
                            let stat = model.statistics
                                newstat = { stat | first = Date ( Just first )
                                                 , last = Date (Just last )
                                          }
                            in
                                ( { model | statistics = newstat }
                                , Cmd.none )
                        _ -> ( model, Cmd.none )
                Err err -> doerr "gotinterval decode" <| JD.errorToString err

        GotInterval ( Err err) -> doerr "gotinterval http" <| U.unwraperror err

        Paste payload ->
            let
                newtimeSeries = applyPastedDict model payload
            in
            U.nocmd ( setOnEdtitionTs model newtimeSeries )

        ClickCell index ->
            case model.holding.shift of
                False -> let ( newmodel, cmd  ) = applyFocus model ( Just index )
                         in
                            ( clearSelection newmodel , cmd )
                True ->
                    case model.focus of
                        Nothing -> applyFocus model ( Just index )
                        Just focus ->
                            let newmodel = setContiguousSelection
                                                model
                                                focus
                                                index
                            in
                                U.nocmd  newmodel

        SelectRow index ->
            let transformed = Dict.map
                                (if model.holding.mouse
                                    then
                                        case model.firstShift of
                                            Nothing -> (\ k v -> v)
                                            Just firstDrag ->
                                                selectContiguous firstDrag index
                                    else
                                        ( flipSelection index )
                                )
                                ( getEditionTs model.series )
                newmodel = setOnEdtitionTs model transformed
            in
                applyFocus ( setupFirstSelected newmodel ) ( Just index )


        DeselectAll keepFocus ->
             let
                 newmodel = ( clearSelection model )
              in
                if keepFocus
                    then applyFocus { newmodel | firstShift = Nothing } model.focus
                    else applyFocus { newmodel | firstShift = Nothing } Nothing


        CopySelection ->
            let selectedValues = getSelectedValues model.series
                concatened = String.join "\n" selectedValues
            in
            ( model
            , Cmd.batch [ deselect True
                        , copyToClipboard concatened
                        ]
            )

        CopyFromBrowser _->
            ( model, T.perform identity ( T.succeed CopySelection ))

        FillNas indexLastValue ->
            let
                lastValue = getLastValue
                                ( getEditionTs model.series )
                                indexLastValue
            in
                applyFocus
                    ( setupNas
                        <| setOnEdtitionTs
                            model
                                <|Dict.union
                                    (fillNas
                                        ( getEditionTs model.series )
                                        lastValue
                                        ( indexLastValue )
                                    )
                                    ( getEditionTs model.series )
                    )
                    Nothing

        FillAll ->
            ( setupNas
                <| setOnEdtitionTs
                        model
                        <| fillAllNas
                            ( getEditionTs model.series )
                            model.lastValids
            , Cmd.none
            )

        NoAction -> U.nocmd model

        ActionControl ( key ) ->
            let holding = model.holding
            in
            case key of
                Escape Down -> ( model , deselect True )
                Escape Up -> U.nocmd model
                Delete  Down -> ( setupNas <| setOnEdtitionTs
                                                model
                                                <| deleteSelectedValues
                                                   model.series

                                , deselect True
                                )
                Delete Up -> U.nocmd model
                Shift Down ->  U.nocmd { model | holding = { holding | shift = True }}
                Shift Up ->  U.nocmd { model | holding = { holding | shift = False }}
                Control Down -> U.nocmd { model | holding = { holding | control = True }}
                Control Up -> U.nocmd { model | holding = { holding | control = False }}
                ArrowDown Down -> arrowAction model 1
                ArrowUp Down -> arrowAction model -1
                PageDown Down -> arrowAction model 30
                PageUp Down -> arrowAction model -30
                Enter Down -> case model.focus of
                    Nothing -> U.nocmd model
                    Just index -> applyFocus model ( Just ( index - 1 ))
                ArrowDown Up -> U.nocmd model
                ArrowUp Up -> U.nocmd model
                PageDown Up -> U.nocmd model
                PageUp Up -> U.nocmd model
                Enter Up -> U.nocmd model
                Other keyName action -> U.nocmd { model | keyName = keyName }

        Drag mode ->
            let holding = model.holding
            in
            case mode of
                On index -> U.nocmd
                                { model | holding = { holding | mouse = True }
                                        , firstShift = Just index
                                }

                Off -> U.nocmd
                        { model | holding = {holding | mouse = False }}


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
                            Nothing ->
                                let newSeries = resetZoom model.series
                                in
                                { model | horizon = { horizonmodel | zoomBounds = Nothing}
                                                    , series = newSeries
                                                    , monotonicCount = model.monotonicCount + 1
                                                    , forceDraw = False
                                                    , statistics = getStatistics
                                                                    model.statistics
                                                                    model.allowInferFreq
                                                                    ( onlyActiveValues  newSeries )
                                        }
                            Just (minDate, maxDate)
                                -> let newSeries =  newZoom
                                                        minDate
                                                        maxDate
                                                        model.series
                                                        model.panActive
                                  in
                                    { model | series = newSeries
                                            , horizon = { horizonmodel | zoomBounds = Just (minDate, maxDate) }
                                            , monotonicCount = model.monotonicCount + 1
                                            , statistics = getStatistics
                                                                model.statistics
                                                                model.allowInferFreq
                                                                ( onlyActiveValues  newSeries )
                                    }

                 in
                    applyFocus ( setupNas newmodel ) ( Just 0 )

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }

        CopyToClipboard copyType->
            let status = model.statusCopy
                ( newStatus, toCopy ) =
                    case copyType of
                        CopyName -> ( { status | name = False }
                                  , model.name)
                        CopyDates -> ( { status | dates = False }
                                   , packDates model.series )
                        CopyValues -> ( { status | values = False }
                                    , packValues model.series )
            in
            ( { model | statusCopy = newStatus }
            , Cmd.batch
                [ copyToClipboard toCopy
                , T.perform (always ( ResetClass copyType )) (P.sleep 1000)
                ]
            )

        ResetClass copyType ->
            let status = model.statusCopy
                newStatus = case copyType of
                                CopyName -> { status | name = True }
                                CopyDates -> { status | dates = True }
                                CopyValues -> { status | values = True }
            in
            U.nocmd { model | statusCopy = newStatus }


applyFocus: Model -> Maybe Int -> ( Model, Cmd Msg )
applyFocus model maybeIndex =
    let
        newModel = { model | focus = maybeIndex }
    in
    case maybeIndex of
        Nothing ->
            case model.focus of
                Nothing -> ( newModel, Cmd.none )
                Just cursor ->
                    ( newModel
                    , T.attempt
                        (\_ -> NoAction)
                            <| Browser.Dom.blur
                                <| idEntry cursor
                    )
        Just index ->
            ( newModel
            , T.attempt
                (\_ -> NoAction)
                <| Browser.Dom.focus
                    <| idEntry index
            )


clearSelection: Model -> Model
clearSelection model =
    let transformed = Dict.map
                        ( \ _ v -> { v | selected = False })
                        ( getEditionTs model.series )
    in
        setOnEdtitionTs
            { model | firstSelected = Nothing
                    , firstShift = Nothing
            }
            transformed


deselect: Bool -> Cmd Msg
deselect keepFocus =
    T.perform identity (T.succeed ( DeselectAll keepFocus ))


idEntry: Int -> String
idEntry = (\ idx -> "e-" ++ ( String.fromInt idx ))


patchWithValue: Dict String Entry -> (String , Edited) -> Maybe String -> Dict String Entry
patchWithValue series (date, edition) raw =
    let
        newentry =
            updateEntry edition <| raw
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


newZoom: String -> String -> Series -> Bool -> Series
newZoom minDate maxDate series pan =
    case series of
        Naked ts -> Naked { initialTs = ts.initialTs
                          , zoomTs = Just <| newZoomT
                                                minDate
                                                maxDate
                                                ts.initialTs
                                                ts.zoomTs
                                                pan
                          }
        ToEdit ts -> ToEdit { initialTs = ts.initialTs
                            , zoomTs = Just <| newZoomT
                                                minDate
                                                maxDate
                                                ts.initialTs
                                                ts.zoomTs
                                                pan
                            }


newZoomT: String -> String -> Dict String e -> Maybe ( Dict String e ) ->  Bool -> Dict String e
newZoomT minDate maxDate initial zoom pan =
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



justValues: Dict String ( Maybe Float ) -> List Float
justValues series =
     List.concat
        <| List.map
            (\ (k, v) -> case v of
                            Nothing -> []
                            Just val -> [ val ]
            )
            (Dict.toList series)


resetZoom: Series -> Series
resetZoom series =
    case series of
        Naked ts -> Naked { initialTs = ts.initialTs
                          , zoomTs = Nothing}
        ToEdit ts -> ToEdit { initialTs = ts.initialTs
                            , zoomTs = Nothing}

onlyValues: SeriesToEdit -> Dict String ( Maybe Float )
onlyValues series =
    Dict.map
        (\ k e -> e.value )
        series

onlyActiveValues : Series -> Dict String ( Maybe Float )
onlyActiveValues series =
    case series of
        Naked ts ->
            case ts.zoomTs of
                Nothing -> ts.initialTs
                Just zoom -> zoom
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> onlyValues ts.initialTs
                Just zoom -> onlyValues zoom


onlyActiveKeys : Series -> List String
onlyActiveKeys series =
    case series of
        Naked ts ->
            case ts.zoomTs of
                Nothing -> Dict.keys ts.initialTs
                Just zoom -> Dict.keys zoom
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> Dict.keys ts.initialTs
                Just zoom -> Dict.keys zoom


getEditionTs: Series -> SeriesToEdit
getEditionTs series =
    case series of
        Naked ts -> Dict.empty
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> ts.initialTs
                Just zoom -> zoom

setOnEdtitionTs: Model -> SeriesToEdit -> Model
setOnEdtitionTs model patch =
    case model.series of
        Naked _ -> model
        ToEdit series ->
            let ts = case series.zoomTs of
                        Nothing -> { initialTs = patch
                                    , zoomTs = Nothing }
                        Just _ -> { initialTs = series.initialTs
                                  , zoomTs = Just patch }
            in
            { model | series = ToEdit ts }


getLastNaive: List String -> String
getLastNaive dates =
    case List.maximum dates of
        Nothing -> "No Last"
        Just lastDate
            -> let mNaive = List.head
                            <| String.split
                                "+"
                                lastDate
               in case mNaive of
                   Nothing -> "No Last"
                   Just naive -> String.replace "T" " " naive


cropTs: Dict String e -> (String, String) ->  Dict String e
cropTs ts bounds =
    let naive = getLastNaive ( Dict.keys ts )
    in
       if naive == ( Tuple.second bounds )
            then removeLast ts
            else ts


removeLast: Dict String e -> Dict String e
removeLast series =
    case List.reverse ( Dict.toList series ) of
        [] -> series
        x :: xs -> Dict.fromList ( List.reverse xs )


packDates: Series -> String
packDates series =
    String.join("\n")
        ( onlyActiveKeys series )

myFloat mf =
 case mf of
    Nothing -> ""
    Just f -> String.fromFloat f


packValues: Series -> String
packValues series =
    String.join("\n")
        <| List.map
            myFloat
            ( Dict.values ( onlyActiveValues series ))


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


getInterval: String -> String -> Cmd Msg
getInterval baseUrl name =
      Http.get
        { url =
               UB.crossOrigin
               baseUrl
                [ "api", "series", "metadata" ]
                [ UB.string "name" name
                , UB.string "type" "interval" ]
        , expect = Http.expectString GotInterval
        }


getStatistics: Statistics -> Bool -> Dict String ( Maybe Float )-> Statistics
getStatistics previous allowInfer series =
    let dates = List.sort ( Dict.keys series )
        values = List.sort <| justValues series
        length = List.length values
    in
        { previous
            |start = Date <| List.head dates
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


getCurrentValue: Entry -> Maybe Float
getCurrentValue entry =
    case entry.edited of
        Edition value -> Just value
        Deletion -> Nothing
        NoEdition -> entry.value
        Error _ -> Nothing


getSelectedValues: Series -> List String
getSelectedValues series =
        List.map
            (\ e ->  case (getCurrentValue e) of
                        Nothing -> ""
                        Just val -> String.fromFloat val
            )
            <| List.filter
                (\ e -> e.selected )
                ( Dict.values ( getEditionTs series ))


deleteSelectedValues: Series -> SeriesToEdit
deleteSelectedValues series =
    Dict.map
        (\ k e ->  if e.selected
                    then
                        { e | edited = Deletion
                            , raw = Nothing
                        }
                    else e
        )
        ( getEditionTs series )


setupNas: Model -> Model
setupNas model =
    { model | lastValids = findLastValid
                            ( Dict.toList
                                ( getEditionTs model.series )
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


setupFirstSelected: Model -> Model
setupFirstSelected model =
    let ( _, first ) = firstSelected ( Dict.values (getEditionTs model.series))
    in
        { model | firstSelected = first }


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


selectContiguous: Int -> Int -> ( a -> Entry -> Entry )
selectContiguous from to =
        if to < from
            then   ( \ _ v ->
                        { v | selected =
                            ( ( v.index >= to ) && ( v.index <= from ))
                        }
                    )
            else
                    ( \ _ v ->
                        { v | selected =
                            ( ( v.index <= to ) && ( v.index >= from ))
                        }
                    )


setContiguousSelection: Model -> Int -> Int -> Model
setContiguousSelection model from index =
    setOnEdtitionTs
        model
        <| Dict.map
            ( selectContiguous from index )
            ( getEditionTs model.series )


flipSelection : Int -> (a -> Entry -> Entry )
flipSelection index =
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


bounded: Int -> Int -> Int -> Int
bounded minValue maxValue value =
    if value < minValue
        then minValue
        else if value > maxValue
            then maxValue
            else value


getLimitIndexes: Dict String Entry -> ( Int, Int )
getLimitIndexes series =
    let indexes = List.map
                    ( \ e -> e.index )
                    <| Dict.values series
    in
    ( Maybe.withDefault 0 ( List.minimum indexes )
    , Maybe.withDefault 0 (List.maximum indexes )
    )


arrowAction: Model -> Int -> ( Model, Cmd Msg )
arrowAction model increment =
    let ( newmodel, cmd ) = arrowActionT model increment
    in
        ( setupFirstSelected newmodel , cmd )


arrowActionT: Model -> Int -> ( Model, Cmd Msg )
arrowActionT model increment =
      case model.focus of
        Nothing -> U.nocmd model
        Just focus ->
            let ( minIndex, maxIndex ) = getLimitIndexes
                                            ( getEditionTs model.series )
                bound = bounded minIndex maxIndex
            in
            case model.holding.shift of
                False -> case model.holding.control of
                            False -> applyFocus
                                        model
                                        ( Just ( bound ( focus + increment )))
                            True -> case increment > 0 of
                                        True -> applyFocus
                                                    model
                                                    ( Just maxIndex )
                                        False -> applyFocus
                                                    model
                                                    ( Just minIndex )
                True ->
                    case model.holding.control of
                        False ->
                            case model.firstShift of
                                Nothing ->
                                    applyFocus
                                         ( setContiguousSelection
                                                { model | firstShift = Just focus
                                                }
                                                ( bound ( focus + increment ))
                                                focus
                                         )
                                         ( Just ( bound ( focus + increment )))
                                Just firstShift ->
                                     applyFocus
                                         ( setContiguousSelection
                                                { model | focus =  Just ( bound (focus + increment ))}
                                                ( bound ( focus + increment ))
                                                ( firstShift )
                                         )
                                         ( Just ( bound (focus + increment ) ))
                        True ->
                            let relevantBound = if increment > 0
                                                    then maxIndex
                                                    else minIndex
                            in
                            case model.firstShift of
                                Nothing ->
                                    applyFocus
                                         ( setContiguousSelection
                                                { model | firstShift = Just focus
                                                }
                                                relevantBound
                                                focus
                                         )
                                         ( Just relevantBound)
                                Just firstShift ->
                                     applyFocus
                                         ( setContiguousSelection
                                                { model | focus =  Just relevantBound }
                                                relevantBound
                                                ( firstShift )
                                         )
                                         ( Just relevantBound )


getRelevantData : Model -> List (Cmd Msg)
getRelevantData model =
    case model.mode of
        Existing I.Primary ->
            [ getPoints model
            , I.getidates model "series" InsertionDates
            ]
        Existing I.Formula ->
            [ getPoints model ]
        Creation _ -> [ Cmd.none ]


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
        editionTs = getEditionTs model.series
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
                    editionTs
                )
        listIndex =
            List.range
                firstIndex
                (firstIndex + (List.length newValues) - 1)
        listDates =
            Dict.keys
                (Dict.filter
                     (\_ value -> List.member value.index listIndex)
                     editionTs
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
        editionTs
        copyPastedDict
        editionTs


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
                 ([ ("name", JE.string model.name )
                 , ("author" , JE.string "webui" )
                 , ("tzaware", JE.bool tzaware )
                 , ("series", encodeEditedData patch )
                 , ("supervision", JE.bool True )
                 , ("keepnans", JE.bool True)
                 ] ++ if tzaware
                        then [( "tzone", JE.string model.horizon.timeZone )]
                        else []
                 )

        , headers = [ ]
        , timeout = Nothing
        , tracker = Nothing
        , url = UB.crossOrigin model.baseurl
                [ "api", "series", "state" ] [ ]
        , expect = Http.expectString Saved
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
        [ H.text "permalink"]


maybeRoundForm: Model -> List ( H.Html Msg )
maybeRoundForm model =
    case model.mode of
        Existing I.Primary ->  []
        Existing I.Formula ->
            [ H.div
                [ HA.class "form-round"]
                [ H.text "Decimals : "
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
        Creation _ -> []


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
        None ->
            "Save"
        Failure ->
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


buttonFillAll: Model -> H.Html Msg
buttonFillAll model =
    if model.lastValids /= []
        then H.button
                    [ HA.class "button-fill-all"
                    , HE.onClick FillAll
                    , HA.title "Forward-fill on all selection"
                    ]
                    [ H.text "All ↓" ]
        else
             H.div [HA.class "button-fill-all"] []


viewRelevantTable: Model -> H.Html Msg
viewRelevantTable model =
    case model.mode of
        Existing I.Primary -> viewEditTable model
        Existing I.Formula -> viewValueTable model
        Creation Form -> H.div [] [ nameForm model, creationForm model ]
        Creation Edit -> H.div [] [ nameForm model, viewEditTable model ]


checkMandatory: String -> String
checkMandatory s =
    if s == ""
    then "mandatory"
    else ""

className: NameStatus -> String
className status =
    case status of
        Missing -> "mandatory"
        Invalid -> "invalid"
        Valid -> "valid"


nameForm: Model -> H.Html Msg
nameForm model =
    H.fieldset
        []
        [ H.label
            [ HA.for "creation-name"]
            [ H.text "Name: " ]
        , H.input
            [ HA.id "creation-name"
            , HA.class ( className model.creation.nameStatus )
            , HA.name "name"
            , HA.autocomplete False
            , HE.onInput ( \s -> Create ( Name s ) )
            , HA.value model.creation.name
            ]
            []
        ]

creationForm: Model -> H.Html Msg
creationForm model =
    H.div
        []
        [ H.fieldset
            []
            [ H.label
                [HA.for "creation-from"]
                [H.text "From: "]
            , H.input
                [ HA.type_ "date"
                , HA.id "creation-from"
                , HA.class  ( checkMandatory model.creation.from )
                , HA.name "from"
                , HE.onInput ( \s -> Create ( From s ) )
                , HA.value model.creation.from
                ]
                []
            , H.label
                [HA.for "creation-to"]
                [H.text "To: "]
            , H.input
                [ HA.type_ "date"
                , HA.id "creation-to"
                , HA.class  ( checkMandatory model.creation.to )
                , HA.name "to"
                , HE.onInput ( \s -> Create ( To s ) )
                , HA.value model.creation.to
                ]
                []
            , H.label
                [ HA.for "creation-tz" ]
                [ H.text "Timezone: " ]
            , tzoneDropdown model.horizon.timezones model.creation.tz model.horizon.timeZone
            ]
        , H.fieldset
            []
            [ H.label
                [ HA.for "creation-multiplier"]
                [ H.text "Frequency: " ]
            , H.input
                [ HA.id "creation-multiplier"
                , HA.type_ "number"
                , HA.min "1"
                , HA.step "1"
                , HA.name "multiplier"
                , HE.onInput ( \s -> Create ( FreqMultiply s ) )
                , HA.value ( case model.creation.freq.multiplier of
                                Nothing -> ""
                                Just n -> String.fromInt n
                           )
                ]
                []
            , H.label
                [ HA.for "creation-offset"]
                [ ]
            , H.input
                [ HA.id "creation-offset"
                , HA.class ( checkMandatory model.creation.freq.offset )
                , HA.name "offset"
                , HE.onInput ( \s -> Create ( FreqOffset s ) )
                , HA.value model.creation.freq.offset
                ]
                []
            ]
        , H.fieldset
            []
            [ H.label
                [ HA.for "creation-value"]
                [ H.text "Value: " ]
            , H.input
                [ HA.id "creation-value"
                , HA.type_ "number"
                , HA.name "value"
                , HE.onInput ( \s -> Create ( Value s ) )
                , HA.value ( case model.creation.value of
                                Nothing -> ""
                                Just f -> String.fromFloat f
                           )
                ]
                []
            ]
        , H.button
            [ HE.onClick ( Create Preview )
            , HA.disabled ( not model.creation.mandatoryValid )
            , HA.class "bluebutton custom-button"
            ]
            [ H.text "Preview" ]
        ]


tzoneDropdown : List String-> TzSelector -> String -> H.Html Msg
tzoneDropdown choices selected fromHorizon=
    let
        decodeTimeZone : String -> JD.Decoder Msg
        decodeTimeZone timeZone =
            JD.succeed (Create (Tz timeZone))

    in
    H.select
        [ HE.on "change" (JD.andThen decodeTimeZone HE.targetValue)
        , HA.id "creation-tz"
        , HA.name "tz"
        ]
        ( List.map (renderTimeZone selected fromHorizon ) ( naiveTag::choices ))


renderTimeZone : TzSelector -> String -> String -> H.Html msg
renderTimeZone selectedTz fromHorizon timeZone =
    case selectedTz of
        Selected tz ->
            H.option
                [ HA.value timeZone
                , HA.selected ( tz == timeZone )
                ]
                [ H.text timeZone ]
        Unchanged ->
             H.option
                [ HA.value timeZone
                , HA.selected ( fromHorizon == timeZone )
                ]
                [ H.text timeZone ]
        Naive ->
             H.option
                [ HA.value timeZone
                , HA.selected ( naiveTag == timeZone )
                ]
                [ H.text timeZone ]



viewValueTable: Model -> H.Html Msg
viewValueTable model =
    H.table
        [HA.class "table talbe-sm show-table"]
          [ headerShowValue model
          , bodyShowValue model ]


bodyShowValue: Model -> H.Html Msg
bodyShowValue model =
    let nbPoints = List.length ( onlyActiveKeys model.series )
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
        <| Dict.keys
            ( onlyActiveValues model.series )


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
    let allDates = onlyActiveKeys
                        <| Maybe.withDefault
                            emptySeries
                                <| Dict.get
                                    comp.name
                                    model.componentsData
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
                [HA.class "show-table-dates"]
                [ H.text date ]
            , H.td [] [ H.text
                            <| printValue
                                model.roundValues
                                <| Maybe.withDefault
                                    Nothing
                                    <| Dict.get
                                            date
                                            ( onlyActiveValues model.series )
                        ]
            ]
            ( addComponentCells model date ) )


headerShowValue: Model -> H.Html Msg
headerShowValue model =
    H.thead
        []
        [ H.tr
            []
            ( [ H.th
                [ HA.class "show-table-dates" ]
                 [ H.p
                    [ HA.class <| getCopyClass
                                    model.statusCopy
                                    CopyDates
                    , HE.onClick ( CopyToClipboard CopyDates )
                    , HA.class "copy-all"
                    , HA.title "Copy dates"
                    ]
                    []
                , H.p [] [ H.text "Dates" ]             ]
              , H.th
                    [ HA.class "show-table-series" ]
                    [ H.p
                        [ HA.class <| getCopyClass
                                        model.statusCopy
                                        CopyValues
                        , HE.onClick ( CopyToClipboard CopyValues )
                        , HA.class "copy-all"
                        , HA.title "Copy values"
                        ]
                        []
                    , H.p [] [ H.text "Values" ]
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
        []
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
                                        <| onlyActiveValues
                                            <| Maybe.withDefault
                                                emptySeries
                                                  <|Dict.get
                                                        comp.name
                                                        model.componentsData
                            ]
        )
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
    let negative = String.startsWith "-" number
        absolute = if negative
                    then String.replace "-" "" number
                    else number
        parts = String.split "." absolute
    in
      case parts of
          [] -> ""
          [x] ->  ( restoreSign negative )
                    <| String.reverse
                        <| addSpace
                            <| String.reverse x
          x :: xs ->
            String.concat
                [( restoreSign negative )
                    <| String.reverse
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


restoreSign: Bool -> String -> String
restoreSign negative number =
    if negative
        then String.concat [ "-",  number ]
        else number


currentDiff: Model -> Dict String Entry
currentDiff model =
    Dict.map
        (\ _ e -> { e | edited = ( linearCorrection model e.edited )})
        ( Dict.filter
            (\_ entry -> entry.edited  /= NoEdition )
            ( getEditionTs model.series )
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
        [ editTable model
        , commonHeaderEdition model patch
        ]


commonHeaderEdition : Model -> Dict String Entry -> H.Html Msg
commonHeaderEdition model patch =
    H.div
        [ HA.class "save-table" ]
        [ H.div
          [ HA.class "for-current-diff"]
          ( divLinearCorrection model patch ++
            saveButtons model patch )
        , divSaveDataTable patch
        ]



saveButtons: Model -> Dict String Entry -> List (H.Html Msg)
saveButtons model patch =
    if Dict.isEmpty patch
        then [ ]
        else
            [ H.div
                [ HA.class "save-buttons" ]
                [ H.button
                  [ HA.class "bluebutton custom-button"
                  , HA.attribute "type" "button"
                  , HE.onClick CancelEdition
                  , HA.disabled (model.horizon.plotStatus == Loading)
                  ]
                  [ H.text "Cancel" ]
                , H.button
                  [ HA.class "greenbutton custom-button"
                  , HA.attribute "type" "button"
                  , HE.onClick SaveEditedData
                  , HA.disabled (  model.horizon.plotStatus == Loading
                                || model.creation.nameStatus == Invalid
                                || model.creation.nameStatus == Missing
                                )
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
        class = HA.class "grid-edit-table"
    in
    case statePoints
            (Dict.size ( getEditionTs model.series ))
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
                      [ HA.class "edit-table" ]
                      [ H.thead [ ]
                            [ H.tr [ ]
                                  [ H.th
                                        [ HA.scope "col"
                                        , HA.class "col-dates"]
                                        [ H.p
                                            [ HA.class <| getCopyClass
                                                            model.statusCopy
                                                            CopyDates
                                            , HE.onClick ( CopyToClipboard CopyDates )
                                            , HA.class "copy-all"
                                            , HA.title "Copy dates"
                                            ]
                                            []
                                      , H.p [] [ H.text "Dates" ]
                                        ]
                                  , H.th
                                      [ HA.scope "col"
                                      , if model.lastValids == []
                                            then HA.class "no-fill"
                                            else HA.class "fill"
                                      , HA.class "col-values"]
                                      [ H.div
                                        []
                                        [ H.p
                                            [ HA.class <| getCopyClass
                                                            model.statusCopy
                                                            CopyValues
                                            , HE.onClick ( CopyToClipboard CopyValues )
                                            , HA.class "copy-all"
                                            , HA.title "Copy values"
                                            ]
                                            []
                                        , H.p [] [ H.text "Values" ]
                                        ]
                                      , buttonFillAll model
                                      ]
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
                                <| Dict.toList
                                    ( getEditionTs model.series )
                      ]
                , nodeTriggerPastable model
                ]


divSaveDataTable : Dict String Entry -> H.Html Msg
divSaveDataTable filtredDict =
    if Dict.isEmpty filtredDict then
        H.div [] []
    else
        H.div
            [HA.class "grid-save-table"]
            [ H.table
                  [ HA.class "edit-table" ]
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
                                Edition val -> formatNumber <|
                                                    String.fromFloat val
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
                , H.text ".X + "
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
        focused = entry.index == Maybe.withDefault -1 model.focus
        fillUnder = List.member entry.index  model.lastValids
    in
    H.tr
        ([ HA.class "row-edit"
        , if entry.selected
            then HA.class "selected"
            else HA.class ""
        , if focused
            then HA.class "focused"
            else HA.class ""
        , HE.onClick ( ClickCell entry.index )
        , HE.onDoubleClick (SelectRow entry.index)
        , HE.onMouseDown ( Drag ( On entry.index ))
        ] ++
            if model.holding.mouse
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
                  [ HA.id (idEntry entry.index )
                  , HA.class ("pastable " ++ rowstyle)
                  , if fillUnder then HA.class "fill-under" else HA.class "plain"
                  , HA.placeholder "enter your value"
                  , HA.autocomplete False
                  , HA.value ( formatNumber ( getValue entry ))
                  , HE.onInput (InputChanged date)
                  , HA.attribute "index" date
                  , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                  ]
                  [ ]
             ] ++ ( fillButton entry fillUnder
                  )
            )
         ] ++ (buttonsFirstSelected isFirstSelected )
        )


fillButton: Entry -> Bool -> List ( H.Html Msg )
fillButton entry fillUnder =
    case fillUnder of
        True -> [ H.button
                    [ HA.title "Fill"
                    , HE.onClick ( FillNas ( entry.index ) )]
                    [ H.text "↓" ]
                ]
        False -> []

buttonsFirstSelected: Bool -> List (H.Html Msg)
buttonsFirstSelected predicat =
    if not predicat
        then []
        else [
            H.td
                [ HA.class "control-col"
                , HA.class classClip
                , HA.class "copy-selection"
                , HA.title "Copy selection"
                , HE.onClick CopySelection
                ]
                [ ]
            , H.td
                [ HA.class "control-col"
                , HA.class "remove-selection"
                , HA.title "Deselect all"
                , HE.onClick ( DeselectAll False )
                ]
                [ H.text "x"]
            ]


isEmpty: Model -> Bool
isEmpty model =
    List.length ( onlyActiveKeys model.series ) == 0


debugView: Model -> H.Html Msg
debugView model =
    H.div
        []
        ( if model.horizon.debug
            then
                [ H.pre []
                ( [ H.text " debug active "
                , H.br [] []
                , H.text ("Last Date: " ++ ( getLastNaive <| case model.series of
                                                                Naked series -> Dict.keys series.initialTs
                                                                ToEdit series -> Dict.keys series.initialTs
                                            )
                          )
                , H.text (", dragMode: " ++ if model.holding.mouse
                                                    then "On"
                                                    else "Off")
                , H.br [] []
                , H.text (", shiftHold: " ++ if model.holding.shift
                                                    then "On"
                                                    else "Off")
                , H.br [] []
                , H.text (", controlHold: " ++ if model.holding.control
                                                    then "On"
                                                    else "Off")
                , H.br [] []
                , H.br [] []
                , H.text  ( ", Key Pressed: " ++ model.keyName)
                , H.text  ( ", Focus : " ++ case model.focus of
                                                Nothing -> "Nothing"
                                                Just focus -> String.fromInt focus
                                )

                , H.text  ( ", FirstShift: " ++ case model.firstShift of
                                Nothing -> "Nothing"
                                Just focus -> String.fromInt focus
                        )
                ] ++ ( List.map
                            (\ i -> H.text (", Na to fill at: " ++ String.fromInt i ))
                            model.lastValids
                       )
                  ++ [ H.text "Errors: " ]
                  ++  ( List.map
                            (\ e -> H.text e)
                            model.errors
                       )
               )
               ]
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

rowStat : Int -> String -> Maybe String -> TypeStat -> H.Html Msg
rowStat round name mTitle statistic  =
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
             ( case mTitle of
                    Nothing ->
                        [ ]
                    Just title ->
                        [ HA.title title ]
            )
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
        , partialRow "First" ( Just "Lower bound of whole series" ) model.statistics.first
        , partialRow "Last" ( Just "Upper bound of whole series" ) model.statistics.last
        , partialRow "Start" ( Just "Lower bound of selected horizon" ) model.statistics.start
        , partialRow "End" ( Just "Upper bound of selected horizon" ) model.statistics.end
        , partialRow "Min" Nothing model.statistics.min
        , partialRow "Max" Nothing model.statistics.max
        , partialRow "Sum" Nothing model.statistics.sum
        , partialRow "Count" Nothing model.statistics.count
        , partialRow "NaNs" Nothing model.statistics.nas
        , partialRow "Mean" Nothing model.statistics.mean
        , partialRow "P25" Nothing model.statistics.p25
        , partialRow "P50" Nothing model.statistics.median
        , partialRow "P75" Nothing model.statistics.p75
        , partialRow "Freq" Nothing model.statistics.inferFreq
        ]


displayStatus: Model -> H.Html Msg
displayStatus model =
    if model.horizon.plotStatus == None
      then H.text ""
      else if isEmpty model && (model.horizon.plotStatus == Success)
           then H.text """It seems there is no data to display in this
                        interval, select another one."""
           else H.text ""


plotNode: Model -> H.Html Msg
plotNode model =
    let dates = onlyActiveKeys model.series
        values = Dict.values ( onlyActiveValues model.series )
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
                        "markers"
                        defaultTraceOptions
                    ]
                    { defaultLayoutOptions | dragMode = Just dragMode
                                           , yaxis = newYaxis
                                           , height = Just 350
                    }
                    defaultConfigOptions
                )
            ]
            [ ]


view : Model -> H.Html Msg
view model =
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
                        ( Just ( permaLink model ))
                        False
                        "Series Editor"
                        ( getFromToDates model.horizon )
            , I.viewtitle
                model
                ( getCopyClass model.statusCopy CopyName )
                ( CopyToClipboard CopyName )
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


commandStart: Model -> Cmd Msg
commandStart model =
    case model.mode of
        Existing _ -> Cmd.batch
                        [ M.getsysmetadata
                            model.baseurl
                            model.name
                            GotMetadata
                            "series"
                        , getsource model.baseurl model.name
                        ]
        Creation _ -> getCatalog model


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
                    , mode = if input.name == ""
                                then Creation Form
                                else Existing I.Primary
                    , meta = Dict.empty
                    , catalog = Nothing
                    , source = ""
                    , seriestype = I.Primary
                    , horizon = initHorizon
                                    input.baseurl
                                    input.min
                                    input.max
                                    input.debug
                                    None
                    , creation = initCreationModel
                    , initialCommands = Cmd.none
                    , forceDraw = False
                    , allowInferFreq = False
                    , intercept = Nothing
                    , slope = Nothing
                    , insertion_dates = Array.empty
                    , processedPasted = [ ]
                    , monotonicCount = 0
                    , rawPasted = ""
                    , focus = Nothing
                    , firstShift = Nothing
                    , firstSelected = Nothing
                    , holding = emptyHolding
                    , keyName = ""
                    , lastValids = []
                    , series = emptySeries
                    , statistics = emptyStat
                    , roundStat = 2
                    , roundValues = Nothing
                    , statusCopy = initialStatusCopy
                    , panActive = False
                    , components = []
                    , componentsData = Dict.empty
                    }
    , Cmd.none
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
                , onKeyDown ( keyDecoder Down )
                , onKeyUp ( keyDecoder Up )
                , loadLocal FromLocal
                , loadFromLocalStorage
                    (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
                ]
        }

