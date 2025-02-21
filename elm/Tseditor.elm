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
import Html exposing (Attribute)
import Html.Attributes as HA
import Html.Events as HE
import Info as I
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required, hardcoded)
import List.Extra as List
import List.Statistics as Stat
import Markdown
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
            Typing action char
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
    if keyValue == "Meta"
    then Meta action
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
    if keyValue == "ArrowLeft"
    then ArrowLeft action
    else
    if keyValue == "ArrowRight"
    then ArrowRight action
    else
    if keyValue == "PageDown"
    then PageDown action
    else
    if keyValue == "PageUp"
    then PageUp action
    else
    if keyValue == "Enter"
    then Enter action
    else Other keyValue action


type Action =
    Up
    | Down


type ControlKey =
    Escape Action
    | Delete Action
    | Control Action
    | Meta Action
    | Shift Action
    | ArrowUp Action
    | ArrowDown Action
    | ArrowLeft Action
    | ArrowRight Action
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
    , exist: Bool
    , source : String
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
    , statusCopy : StatusCopy
    , panActive : Bool
    -- user actions
    , forceDraw : Bool
    , allowInferFreq : Bool
    , showDiff: Bool
    -- cells interactivity
    , selection : Maybe Box
    , focus : Maybe ( Int, Int )
    , firstShift: Maybe ( Int, Int )
    , firstSelected : Maybe ( Int, Int )
    , lastValids: List ( Int, Int )
    , currentInput : Maybe ( Int, Int )
    , slope: Maybe String
    , intercept: Maybe String
    -- keyboard/mouse
    , keyName: String
    , holding: Holding
    -- show-values for formula
    , components : List Component
    , coordData: Dict ( Int, Int ) Entry
    , dates: List String
    , columns: List String
    , diff : Dict ( Int, Int ) Entry
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotValueData (Result Http.Error String)
    | GotComponents (Result Http.Error String)
    | GotComponentData CType String (Result Http.Error String)
    | GotGenerated (Result Http.Error String)
    | GotMetadata (Result Http.Error String) -- first command fired
    | GotSource (Result Http.Error String)
    | GotCatalog (Result Http.Error String)
    | GotOffsets (Result Http.Error String)
    | HasCache ( Result Http.Error String )
    | Horizon ModuleHorizon.Msg
    | Create CreationOptions
    | SwitchForceDraw
    | AllowInferFreq
    | ShowDiff
    | InputChanged Int Int String
    | SaveEditedData
    | Saved (Result Http.Error String)
    | CancelEdition
    | Correction Parameter
    | Paste PasteType
    | SelectRow ( Int, Int )
    | DeselectAll Bool
    | ClickCell Int Int
    | UnFocus
    | ActivateCell Int Int
    | Drag DragMode
    | CopySelection
    | CopyFromBrowser Bool
    | CopyToClipboard CopyType
    | ResetClass CopyType
    | ActionControl ControlKey
    | Typing Action Char
    | NoAction
    | FillNas ( Int, Int)
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

type alias Box =
    { t: Int
    , r: Int
    , b: Int
    , l: Int
    }

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
    , catalog : Maybe ( List String )
    , offsets : List String
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
    , freq = { offset = "h"
             , multiplier = Nothing }
    , tz = Unchanged
    , value = Nothing
    , name = ""
    , nameStatus = Missing
    , mandatoryValid = False
    , catalog = Nothing
    , offsets = []
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

type TypeStat =
    Numeric ( Maybe Float )
    | Count Int
    | Date ( Maybe String )
    | InferFreq Freq

type Freq =
    Blocked
    | Authorised ( Maybe String )


type alias Holding =
    { mouse: Bool
    , control: Bool
    , shift : Bool
    , meta: Bool
    }

emptyHolding: Holding
emptyHolding =
    { mouse = False
    , control = False
    , shift = False
    , meta = False
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


type alias BaseSupervision =
    { value : Maybe Float
    , override : Bool
    }


type alias Entry =
    { raw : Maybe String
    , value : Maybe Float
    , edition : Edited
    , editable: Bool
    , override : Bool
    , indexRow: String
    , indexCol: String
    }


baseToEntry: BaseSupervision -> Entry
baseToEntry base =
     { raw = Maybe.map String.fromFloat base.value
        , value = base.value
        , edition = NoEdition
        , editable = True
        , override = base.override
        , indexRow = ""
        , indexCol = ""
    }


emptyEntry : Entry
emptyEntry =
    { raw = Nothing
    , value = Nothing
    , edition = NoEdition
    , editable = False
    , override = False
    , indexRow = ""
    , indexCol = ""
    }


asEdited: Maybe Float -> Edited
asEdited value =
    case value of
        Nothing -> Deletion
        Just v -> Edition v


dressSeries: SeriesNaked -> String -> SeriesToEdit
dressSeries series name =
    Dict.fromList
        <| List.indexedMap
            (\ idx (k, v) -> ( k
                             , { value = v
                               , override = False
                               , edition = asEdited v
                               , editable = True
                               , raw = case v of
                                        Nothing -> Nothing
                                        Just val -> Just
                                                      <| String.fromFloat val
                               , indexCol = name
                               , indexRow = k
                               }
                             )
            )
            ( Dict.toList series )


nameSeries : Dict ( Int, Int ) Entry -> String -> Dict ( Int, Int ) Entry
nameSeries series name =
     Dict.map
        ( \ _ e ->  { e | indexCol = name } )
        series


likeComp: Model -> Component
likeComp model =
    Component
        model.name
        ( asCType model.seriestype )
        model.series
        ( case Dict.get "tzaware" model.meta of
                Just (M.MBool val) -> val
                _ -> False
        )


type Edited =
    Edition Float
    | NoEdition
    | Deletion
    | Error String


-- would need source & last insertion date field
type alias Component =
    { name: String
    , cType: CType
    , data: Series
    , tzaware: Bool
    }


asCType: I.SeriesType -> CType
asCType t =
    case t of
        I.Primary -> Primary
        I.Formula -> Formula

type CType =
    Primary
    | Formula
    | Auto


applyType: String -> CType
applyType strType =
    if strType == "auto"
    then Auto
    else if strType == "formula"
    then Formula
    else Primary


type DragMode =
    On ( Int, Int)
    | Off


type Method
    = GET
    | POST

-- pasting data


type alias PasteType =
    { text: String
    , index: String
    }


textDecoder: JD.Decoder String
textDecoder =
     JD.at [ "detail", "text" ] JD.string


indexDecoder: JD.Decoder String
indexDecoder =
     JD.at [ "detail", "index" ] JD.string


pasteWithDataDecoder : JD.Decoder PasteType
pasteWithDataDecoder =
        JD.map2 PasteType textDecoder indexDecoder


separatorReturn raw =
    if String.contains "\r\n" raw then Just "\r\n" -- windows
    else if String.contains "\n" raw then Just "\n" -- unix
    else Nothing


parsePasted : String -> List ( List String )
parsePasted raw =
    let
        removedSpace = String.replace " " "" raw
        sepR =
            separatorReturn raw
    in
    case sepR of
        Nothing ->
            [ splitByTab removedSpace ]
        Just s ->
            List.map
                splitByTab
                <| String.split s removedSpace


-- the index is of the form e-2-3
getPos: String -> ( Int, Int )
getPos s =
    case String.split "-" s of
        [] -> ( 0, 0 )
        [ e ] -> ( 0, 0 )
        [ e, i ] -> ( 0, 0 )
        [ e, i, j ] -> ( Maybe.withDefault
                            0
                            ( String.toInt i ),
                         Maybe.withDefault
                            0
                            ( String.toInt j ))
        _ -> ( 0, 0 )

splitByTab: String -> List String
splitByTab s =
    String.split tab s

tab = "\t"
return = "\n"


-- series decoder

entryDecoder : JD.Decoder BaseSupervision
entryDecoder =
    JD.map2 BaseSupervision
        (JD.field "series" (JD.maybe JD.float))
        (JD.field "markers" JD.bool)


dataDecoder : JD.Decoder (Dict String Entry)
dataDecoder =
    ( JD.dict ( JD.map baseToEntry entryDecoder ))

componentsDecoder: JD.Decoder (List Component)
componentsDecoder =
    JD.list (JD.map4 Component
                ( JD.field "name" JD.string )
                ( JD.map applyType ( JD.field "type" JD.string ))
                ( JD.succeed emptySeries )
                ( JD.field "tzaware" JD.bool )
            )


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


isSingle model =
    case model.mode of
        Creation Edit -> True
        Existing I.Primary -> True
        _ -> False


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
    case component.cType of
        Auto ->
            getSeries
                model
                ( GotComponentData Auto component.name )
                "eval_formula"
                POST
                component.name
        Primary ->
            getSeries
                model
                ( GotComponentData Primary component.name )
                "supervision"
                GET
                component.name
        Formula ->
            getSeries
                model
                ( GotComponentData Formula component.name )
                "state"
                GET
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


getOffsets: Model -> Cmd Msg
getOffsets model =
    Http.get
        { expect = Http.expectString GotOffsets
        , url = UB.crossOrigin model.baseurl
              [ "serve-offsets" ]
              []
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
                                val
                                ( getFetchBounds model.horizon )
                        zoomTs = applyZoom model indexedval
                        series = ToEdit { initialTs = indexedval, zoomTs = zoomTs }
                        statistics = getStatistics
                                        model.statistics
                                        model.allowInferFreq
                                        ( onlyActiveValues series )
                    in
                        applyFocus
                            ( buildCoord
                                ({ model
                                    | series = series
                                    , statistics = statistics
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    indexedval
                                })
                            )
                        ( Just ( 0, 0 ) )

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
                              zoomTs = applyZoom model val
                          in
                          ( ( buildCoord
                            { model | series = Naked { initialTs = ts
                                                     , zoomTs = zoomTs }
                                    , horizon = updateHorizonFromData
                                                model.horizon
                                                ts
                                    , statistics = getStatistics
                                                model.statistics
                                                model.allowInferFreq
                                                ts
                            }
                            )
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

        GotComponentData cType name (Ok rawdata) ->
            case cType of
                Primary ->
                    case JD.decodeString dataDecoder rawdata of
                        Ok val ->
                            let indexedval =
                                    cropTs
                                        val
                                        ( getFetchBounds model.horizon )
                                zoomTs = applyZoom model val
                                newCD = insertComponentData
                                            model.components
                                            name
                                            ( ToEdit { initialTs = indexedval, zoomTs = zoomTs })
                                newModel = { model | components = newCD }
                            in
                                U.nocmd ( buildCoord newModel )
                        Err err -> U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}
                _ ->
                    case JD.decodeString
                        (JD.dict (JD.maybe JD.float))
                        rawdata of
                    Ok val ->  let zoomTs = applyZoom model val
                                   newCD = insertComponentData
                                            model.components
                                            name
                                            ( Naked { initialTs = val, zoomTs = zoomTs })
                                   newModel = { model | components = newCD
                                              }
                               in
                                   U.nocmd ( buildCoord newModel )
                    Err err -> U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}


        GotComponentData cType name (Err _) ->
            U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        GotGenerated ( Ok rawdata ) ->
             case JD.decodeString
                    (JD.dict (JD.maybe JD.float))
                    rawdata of
                Ok val ->  U.nocmd
                            <| applyDiff
                                <| buildCoord
                                    { model | series =
                                                ToEdit { initialTs = dressSeries val model.name
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
                resetModel = buildCoord
                            <| cleanDiff
                            <| { model | horizon = newModelHorizon
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
                                           , nameStatus = case model.creation.catalog of
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
                          , diff = nameSeries model.diff newCreation.name
                          , meta = Dict.fromList
                                    [( "tzaware", M.MBool tzawarness )]
                  }
                , command )


        SwitchForceDraw ->
            applyFocus
                ( buildCoord ( flipForce model ))
                ( Just (0 , 0 ))

        AllowInferFreq ->
            ({ model | allowInferFreq = True
                     , statistics = getStatistics
                                        model.statistics
                                        True
                                        ( onlyActiveValues model.series )
             }
            , Cmd.none
            )

        ShowDiff -> U.nocmd { model | showDiff = not model.showDiff}

        InputChanged row col rawvalue ->
            let
                rawstring = String.replace " " "" rawvalue
                edition = parseInput rawstring
                raw = if rawstring == ""
                            then Nothing
                            else Just rawstring
            in
            ( applyDiff
                <| updateCoordData
                    model
                    ( row, col )
                    raw
                    edition
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
                        patched =
                            Dict.union
                                ( getEditionTs model.series )
                                val

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
            ( { model | horizon = ( setStatusPlot model.horizon Loading )}
            , Cmd.batch
                [ patchEditedData model
                , deselect True
                ]
            )

        CancelEdition ->
            applyFocus
                ( cleanDiff { model | showDiff = False} )
                model.focus

        Correction param ->
            case param of
                Slope value ->  U.nocmd ( applyDiff { model | slope = Just value })
                Intercept value -> U.nocmd ( applyDiff { model | intercept = Just value })

        Saved (Ok _) ->
            case model.mode of
                Existing _ ->
                    ( cleanDiff model
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

                       first = Date <| case Dict.get "left" allmeta of
                                        Just (M.MString val) -> Just val
                                        _ -> Nothing
                       last = Date <| case Dict.get "right" allmeta of
                                        Just (M.MString val) -> Just val
                                        _ -> Nothing
                       stat = model.statistics
                       newstat = { stat | first = first
                                        , last = last
                                 }
                       newmodel = { model | meta = allmeta
                                          , statistics = newstat
                                          , exist = True
                                          , seriestype = seriestype
                                          , mode = case seriestype of
                                              I.Primary -> Existing I.Primary
                                              I.Formula -> Existing I.Formula
                                  }
                   in
                       ( newmodel
                       , Cmd.batch ([ getHasCache newmodel
                                    , model.initialCommands
                                    , getsource model.baseurl model.name
                                    ])
                       )
                Err err ->
                    doerr "gotmeta decode" <| JD.errorToString err

        GotMetadata (Err err) ->
            case err of
                Http.BadStatus code ->
                    if code == 404
                        then U.nocmd { model | exist = False }
                        else doerr "gotmeta http" <| U.unwraperror err
                _ -> doerr "gotmeta http" <| U.unwraperror err

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
                    let creation = model.creation
                    in
                    U.nocmd { model | creation = { creation | catalog = Just catalog}}
                Err err ->
                    doerr "gotcatalog decode" <| JD.errorToString err

        GotCatalog (Err err) ->
            doerr "gotcatalog http" <| U.unwraperror err

        GotOffsets (Ok raw) ->
            case JD.decodeString (JD.list JD.string) raw of
                Ok offsets ->
                    let creation = model.creation
                    in
                    U.nocmd { model | creation = { creation | offsets = offsets}}
                Err err ->
                    doerr "gotoffsets decode" <| JD.errorToString err

        GotOffsets (Err err) ->
            doerr "gotoffsets http" <| U.unwraperror err

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
            let parsed = parsePasted payload.text
                coordPatch = cartesianDataRec parsed [] 0 0 Dict.empty
                corner = getPos payload.index
                merged = pasteRectangle model.coordData coordPatch corner
            in
            U.nocmd <| applyDiff { model | rawPasted = payload.text
                                         , coordData = merged
                                  }

        UnFocus -> U.nocmd { model | focus = Nothing }

        ClickCell iRow iCol ->
            case model.holding.shift of
                False -> let ( newmodel, cmd  ) = applyFocus model ( Just ( iRow, iCol ) )
                         in
                            case model.currentInput of
                                Nothing -> ( clearSelection newmodel , cmd )
                                Just current ->
                                    if current == ( iRow, iCol )
                                    then
                                        ( newmodel , cmd )
                                    else
                                        ( clearSelection newmodel , cmd )
                True ->
                    case model.focus of
                        Nothing -> applyFocus model ( Just ( iRow, iCol ) )
                        Just ( i, j ) ->
                            let newmodel = { model |
                                                selection =
                                                    Just <| extendSelection
                                                            ( Just ( i, j ))
                                                            ( iRow, iCol )
                                            }
                            in
                                U.nocmd newmodel


        ActivateCell iRow iCol ->
            case Dict.get (iRow, iCol ) model.coordData of
                Nothing -> U.nocmd model
                Just entry -> if entry.editable
                                then U.nocmd { model | currentInput = Just ( iRow, iCol ) }
                                else U.nocmd { model | currentInput = Nothing }


        SelectRow position ->
            let selection = if model.holding.mouse
                                then extendSelection
                                        model.firstShift
                                        position

                                else pointToBox position

                newmodel = { model | selection = Just selection }
            in
                applyFocus ( setupFirstSelected newmodel ) ( Just position )


        DeselectAll keepFocus ->
             let
                 newmodel = ( clearSelection model )
              in
                if keepFocus
                    then applyFocus { newmodel | firstShift = Nothing } model.focus
                    else applyFocus { newmodel | firstShift = Nothing } Nothing


        CopySelection ->
            let concatened = cellsToString
                                model.coordData
                                model.selection
            in
            ( model
            , Cmd.batch [ deselect True
                        , copyToClipboard concatened
                        ]
            )

        CopyFromBrowser _->
            ( model, T.perform identity ( T.succeed CopySelection ))

        FillNas position ->
            let
                lastValue = getValueFromIndex
                                model.coordData
                                position
            in
                applyFocus
                    ( applyDiff
                        { model | coordData = fillNas
                                                model.coordData
                                                lastValue
                                                position
                        }
                    )
                    Nothing

        FillAll ->
            ( applyDiff
                { model | coordData = fillAllNas
                                        model.coordData
                                        model.lastValids
                }
            , Cmd.none
            )

        NoAction -> U.nocmd model

        ActionControl ( key ) ->
            let holding = model.holding
                notEditing = model.currentInput == Nothing
                cond = conditionnalAction model notEditing
            in
            case key of
                Escape Down -> ( model , deselect True )
                Escape Up -> U.nocmd model
                Delete  Down -> ( applyDiff ( deleteFocus ( deleteSelectedValues model ))
                                , deselect True
                                )
                Delete Up -> U.nocmd model
                Shift Down ->  U.nocmd { model | holding = { holding | shift = True }}
                Shift Up ->  U.nocmd { model | holding = { holding | shift = False }}
                Control Down -> U.nocmd { model | holding = { holding | control = True }}
                Control Up -> U.nocmd { model | holding = { holding | control = False }}
                Meta Down -> U.nocmd { model | holding = { holding | meta = True }}
                Meta Up -> U.nocmd { model | holding = { holding | meta = False }}
                ArrowDown Down -> cond <| arrowAction model ( 1, 0)
                ArrowUp Down -> cond <| arrowAction model ( -1, 0)
                ArrowLeft Down -> cond <| arrowAction model ( 0, -1)
                ArrowRight Down -> cond <| arrowAction model ( 0, 1)
                PageDown Down -> cond <| arrowAction model ( 30, 0)
                PageUp Down -> cond <| arrowAction model ( -30, 0)
                Enter Down -> case model.focus of
                    Nothing -> U.nocmd model
                    Just ( i , j ) ->
                        case model.currentInput of
                            Nothing -> U.nocmd { model | currentInput = Just ( i , j )}
                            Just active ->
                                if active == ( i , j )
                                then
                                  applyFocus
                                    { model | currentInput = Just ( i + 1 , j )}
                                    ( Just ( ( i + 1 ), j ))
                                else
                                    U.nocmd { model | currentInput = Just ( i , j )}

                ArrowDown Up -> U.nocmd model
                ArrowUp Up -> U.nocmd model
                ArrowLeft Up -> U.nocmd model
                ArrowRight Up -> U.nocmd model
                PageDown Up -> U.nocmd model
                PageUp Up -> U.nocmd model
                Enter Up -> U.nocmd model
                Other keyName action -> U.nocmd { model | keyName = keyName }

        -- when a cell is selected but not in edition mode
        -- typing will trigger the edition mode
        Typing action char ->
            case action of
                Up -> U.nocmd model
                Down ->
                    if model.holding.control || model.holding.meta
                        then U.nocmd model
                        else
                            case model.currentInput of
                                Just active -> U.nocmd model
                                Nothing ->
                                    case model.focus of
                                        Nothing ->  U.nocmd model
                                        Just ( i, j ) ->
                                            let current = Maybe.withDefault
                                                            emptyEntry
                                                            ( Dict.get ( i, j ) model.coordData )
                                            in
                                            if not current.editable
                                                then U.nocmd model
                                                else
                                                 ({ model | currentInput = Just ( i, j ) }
                                                 , T.perform
                                                    identity
                                                    ( T.succeed ( InputChanged i j ( String.fromChar char )))
                                                 )


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
                                    newComponents = List.map
                                                        (\ c -> { c | data = resetZoom c.data})
                                                        model.components
                                in
                                { model | horizon = { horizonmodel | zoomBounds = Nothing}
                                                    , series = newSeries
                                                    , components = newComponents
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
                                                        model.panActive
                                                        model.series

                                       newComponents = List.map
                                                        (\ c -> { c | data = newZoom
                                                                                minDate
                                                                                maxDate
                                                                                model.panActive
                                                                                c.data
                                                                }
                                                        )
                                                        model.components
                                  in
                                    { model | series = newSeries
                                            , components = newComponents
                                            , horizon = { horizonmodel | zoomBounds = Just (minDate, maxDate) }
                                            , statistics = getStatistics
                                                                model.statistics
                                                                model.allowInferFreq
                                                                ( onlyActiveValues  newSeries )
                                    }

                 in
                    applyFocus ( buildCoord newmodel) ( Just ( 0, 0 ) )

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


applyFocus: Model -> Maybe ( Int, Int ) -> ( Model, Cmd Msg )
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
    {  model | firstSelected = Nothing
             , firstShift = Nothing
             , selection = Nothing
             , currentInput = Nothing
        }


deselect: Bool -> Cmd Msg
deselect keepFocus =
    T.perform identity (T.succeed ( DeselectAll keepFocus ))


idEntry: ( Int, Int ) -> String
idEntry ( i, j ) =
    "e-" ++ ( String.fromInt i ) ++ "-"++ ( String.fromInt j )


flipForce: Model -> Model
flipForce model =
    { model | forceDraw = not model.forceDraw }


newZoom: String -> String -> Bool -> Series ->Series
newZoom minDate maxDate pan series =
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


applyZoom: Model -> Dict String a -> Maybe (Dict String a)
applyZoom model series =
    case model.horizon.zoomBounds of
        Nothing -> Nothing
        Just ( min, max ) ->
            Just
                <| Dict.filter
                        (( \k _ -> (( k >= min ) && ( k <= max ))))
                        series

justValues: Dict String ( Maybe Float ) -> List Float
justValues series =
     List.concat
        <| List.map
            (\ (k, v) -> case v of
                            Nothing -> []
                            Just val -> [ val ]
            )
            (Dict.toList series)


onlyValues: SeriesToEdit -> Dict String ( Maybe Float )
onlyValues series =
    Dict.map
        (\ _ e -> e.value )
        series

resetZoom: Series -> Series
resetZoom series =
    case series of
        Naked ts -> Naked { initialTs = ts.initialTs
                          , zoomTs = Nothing}
        ToEdit ts -> ToEdit { initialTs = ts.initialTs
                            , zoomTs = Nothing}

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


cleanDiff: Model -> Model
cleanDiff model =
    setupFill
      { model | diff = Dict.empty
              , coordData = Dict.map
                                (\ _ e -> { e | edition = NoEdition
                                              , raw = Nothing })
                                model.coordData
              , slope = Nothing
              , intercept = Nothing
      }


insertComponentData: List Component -> String -> Series -> List Component
insertComponentData components name data =
    List.map
        (\ comp -> if comp.name == name
                    then { comp | data = data
                         }
                    else comp
        )
        components


getEditionTs: Series -> SeriesToEdit
getEditionTs series =
    case series of
        Naked ts -> Dict.empty
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> ts.initialTs
                Just zoom -> zoom


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
    case entry.edition of
        Edition edition -> Just edition
        Deletion -> Nothing
        NoEdition -> entry.value
        Error _ -> Nothing


applyOnFilter: Dict comparable b -> ( comparable -> Bool ) -> ( b -> b ) -> Dict comparable b
applyOnFilter dict filter action =
    let contractFilter = (\ k _ -> filter k)
        contractAction = (\ _ v -> action v )
        transformed = Dict.map
                        contractAction
                        ( Dict.filter contractFilter dict )
    in
        Dict.union
            transformed
            dict


keyInSelection :  Box -> ( Int, Int ) -> Bool
keyInSelection box ( row, col ) =
    row >= box.t && row <= box.b && col >= box.l && col <= box.r


getSelected: Dict ( Int, Int ) b -> Maybe Box ->  Dict ( Int, Int ) b
getSelected coordData selection =
    case selection of
        Nothing -> Dict.empty
        Just box -> Dict.filter
                        (\ k _ -> ( keyInSelection box) k)
                        coordData


applyOnSelection: Dict ( Int, Int ) b -> ( b -> b ) -> Maybe Box -> Dict ( Int, Int ) b
applyOnSelection coordData action selection =
    case selection of
        Nothing -> coordData
        Just box ->
            applyOnFilter
                coordData
                ( keyInSelection box )
                action


deleteSelectedValues: Model -> Model
deleteSelectedValues model =
    let delete = (\ e ->  if e.editable
                            then { e | edition = Deletion
                                     , raw = Nothing
                                 }
                            else e
                 )
        newCoord = applyOnSelection
                    model.coordData
                    delete
                    model.selection
    in
        { model | coordData = newCoord }


deleteFocus: Model -> Model
deleteFocus model =
    case model.focus of
        Nothing -> model
        Just focus -> {
            model | coordData =
                        applyOnFilter
                            model.coordData
                            ( \ k  -> k == focus)
                            ( \ e -> if e.editable
                                     then
                                        { e | edition = Deletion
                                             , raw = Nothing
                                         }
                                     else e
                            )
                        }


pointToBox: ( Int, Int ) -> Box
pointToBox ( pRow, pCol ) =
    { t = pRow, b = pRow, l = pCol , r = pCol }


extendSelection: Maybe ( Int, Int ) -> ( Int, Int ) -> Box
extendSelection firstShift ( pRow, pCol ) =
    case firstShift of
        Nothing -> pointToBox ( pRow, pCol )
        Just ( sRow, sCol ) ->
            let minCol = min sCol pCol
                maxCol = max sCol pCol
                minRow = min sRow pRow
                maxRow = max sRow pRow
            in
                { t = minRow, b = maxRow,  l = minCol,  r = maxCol }


setupFill model =
    let coordData = model.coordData
        ( ( minRow, maxRow ), ( minCol, maxCol )) = getBounds coordData
    in
    { model | lastValids = List.concat
                            <| List.map
                                ( findLastValidByCol coordData )
                                ( List.range minCol maxCol )
    }


findLastValidByCol: Dict (Int, Int) Entry -> Int -> List (Int, Int)
findLastValidByCol coordData iCol =
    let column = Dict.filter
                        (\ ( _, j ) v -> j == iCol )
                        coordData
        editable = case List.head ( Dict.values column ) of
                    Nothing -> False
                    Just e -> e.editable
    in
        if not editable
        then []
        else
        findLastValidRec
            ( Dict.toList column )
            False
            []


setupFirstSelected model = model

findLastValidRec: List ( (Int, Int ),  Entry ) -> Bool -> List ( Int, Int ) -> List ( Int, Int )
findLastValidRec entries previousIsValue found =
    case entries of
        [] -> found
        ((iRow, iCol ), val ) :: xs ->
            if previousIsValue
                then
                if getCurrentValue val == Nothing
                    then  List.concat [ [ ( iRow - 1
                                          , iCol
                                          )
                                      ]
                                       , ( findLastValidRec xs False found )
                                       ]
                    else ( findLastValidRec xs True found )
                else
                    if getCurrentValue val == Nothing
                        then ( findLastValidRec xs False found )
                        else ( findLastValidRec xs True found )


fillNas: Dict ( Int, Int ) Entry -> Float -> ( Int, Int ) ->  Dict ( Int, Int ) Entry
fillNas coordData lastValue positionLastValue =
    let nbNas = getNbNas coordData positionLastValue
    in
        Dict.map
            ( applyValue positionLastValue lastValue nbNas )
            coordData


applyValue: ( Int, Int ) -> Float  -> Int -> ( Int, Int ) -> Entry -> Entry
applyValue ( rowLastValue, colLastValue) lastValue  nbNas (iRow, iCol) entry =
    if iRow > rowLastValue && iRow <= rowLastValue + nbNas && iCol == colLastValue
        then { entry | edition = Edition lastValue
                     , raw= Just ( String.fromFloat lastValue )}
        else entry


getNbNas: Dict ( Int, Int ) Entry -> ( Int,  Int ) -> Int
getNbNas coordData position =
    let ( iRow, iCol ) = position
        values = List.map
                    isVoid
                    <| Dict.values
                        <| Dict.filter
                            (\ (i, j) _ -> j == iCol && i > iRow )
                            coordData
    in
        findNbNas values 0


isVoid: Entry -> Bool
isVoid entry =
   getCurrentValue entry == Nothing


findNbNas: List Bool -> Int -> Int
findNbNas values nb =
    case values of
        [] -> nb
        x :: xs ->
            case x of
                True -> findNbNas xs ( nb + 1 )
                False -> nb


fillAllNas : Dict ( Int, Int ) Entry -> List ( Int, Int ) -> Dict ( Int, Int ) Entry
fillAllNas coordData idxNas =
    case idxNas of
        [] -> coordData
        x :: xs ->
            let lastValue = getValueFromIndex coordData x
            in
                ( fillAllNas
                    ( fillNas
                        coordData
                        lastValue
                        x
                    )
                    xs
                )


cellsToString: Dict ( Int, Int ) Entry -> Maybe Box -> String
cellsToString coordData selection =
    let selected = getSelected coordData selection
        (( minRow, maxRow ), ( minCol, maxCol )) = getBounds selected
        boundCol = ( minCol, maxCol )
    in
        String.join
            return
            <| List.map
                ( rowToString selected boundCol )
                ( List.range minRow maxRow )


rowToString: Dict ( Int, Int ) Entry -> ( Int, Int )-> Int -> String
rowToString coordData ( minCol, maxCol ) iRow =
    String.join
        tab
        <| List.map
            ( extractValue coordData iRow )
            ( List.range minCol maxCol )


extractValue: Dict ( Int, Int ) Entry -> Int -> Int -> String
extractValue coordData iRow iCol =
    case Dict.get ( iRow, iCol ) coordData of
        Nothing -> ""
        Just entry -> showValue entry


getValueFromIndex: Dict ( Int, Int ) Entry -> ( Int, Int ) -> Float
getValueFromIndex coordData position =
    let
        entry = Maybe.withDefault
                    emptyEntry
                    ( Dict.get position coordData )
    in
        Maybe.withDefault 0 ( getCurrentValue entry )


bounded: ( ( Int, Int ), ( Int, Int )) -> ( Int, Int ) -> ( Int, Int )
bounded ( ( minRow, maxRow ), ( minCol, maxCol )) ( pRow, pCol) =
    (( simpleBound minRow maxRow pRow )
    ,( simpleBound minCol maxCol pCol ))


simpleBound minValue maxValue value =
    if value < minValue
    then minValue
    else if value > maxValue
        then maxValue
        else value


conditionnalAction: Model -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
conditionnalAction model condition action =
    if condition
        then action
        else U.nocmd model


arrowAction: Model -> ( Int, Int ) -> ( Model, Cmd Msg )
arrowAction model increment =
    let ( newmodel, cmd ) = arrowActionT model increment
    in
        ( setupFirstSelected newmodel , cmd )


addP: ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
addP position increment =
    let ( pR, pC ) = position
        ( iR, iC ) = increment
    in ( pR + iR, pC + iC)


extremum: ( Int, Int ) -> ( Int, Int ) -> (( Int, Int ), ( Int, Int )) -> ( Int, Int )
extremum ( fRow, fCol ) ( iRow, iCol ) ( ( minRow, maxRow ), ( minCol, maxCol )) =
    let col = if iCol == 0
                then fCol
                else if iCol > 0
                    then maxCol
                    else minCol
        row = if iRow == 0
                then fRow
                else if iRow > 0
                    then maxRow
                    else minRow
    in
        ( row, col )


arrowActionT: Model -> ( Int, Int ) -> ( Model, Cmd Msg )
arrowActionT model increment =
    let coordDict = model.coordData
        ( ( minRow, maxRow ), ( minCol, maxCol )) = getBounds coordDict
        bound = bounded ( ( minRow, maxRow ), ( minCol, maxCol ))
    in
    case model.focus of
        Nothing -> U.nocmd model
        Just focus ->
            let extrem = extremum focus increment (( minRow, maxRow ), ( minCol, maxCol ))
            in
            case model.holding.shift of
                False -> case model.holding.control of
                            False -> applyFocus
                                        model
                                        ( Just  ( bound ( addP focus increment )))
                            True ->
                                applyFocus
                                    model
                                    ( Just extrem)
                True ->
                    case model.holding.control of
                        False ->
                            case model.firstShift of
                                Nothing ->
                                    applyFocus
                                         { model | firstShift = Just focus
                                                 , selection = Just <| extendSelection
                                                                        ( Just focus )
                                                                        ( bound ( addP focus increment ))
                                         }
                                         ( Just ( bound ( addP focus increment )))
                                Just firstShift ->
                                     applyFocus
                                         { model | firstShift =  Just firstShift
                                                 , selection = Just <| extendSelection
                                                                         ( Just ( bound (addP focus increment )))
                                                                         ( firstShift )
                                         }
                                         ( Just ( bound (addP focus increment ) ))
                        True ->
                            case model.firstShift of
                                Nothing ->
                                    applyFocus
                                         { model | firstShift = Just focus
                                                 , selection = Just <| extendSelection
                                                                         ( Just extrem )
                                                                         focus
                                         }
                                         ( Just extrem)
                                Just ( rowShift, colShift ) ->
                                     applyFocus
                                        { model | selection = Just <| extendSelection
                                                                         (Just extrem)
                                                                         ( rowShift, colShift )
                                         }
                                         ( Just extrem)


getRelevantData : Model -> List (Cmd Msg)
getRelevantData model =
    case model.mode of
        Existing I.Primary ->
            [ getPoints model
            , I.getidates model "series" InsertionDates True
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


cartesianDataRec:  List ( List a ) ->  List a -> Int -> Int -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
cartesianDataRec mergedData currentRow i j myDict =
        if j == 0
        then
            case mergedData of
                [] -> myDict
                row :: rows ->
                    case row of
                        [] -> myDict
                        cell :: cells ->
                            cartesianDataRec
                                rows
                                cells
                                i
                                ( j + 1 )
                                (Dict.insert (i, j) cell myDict)

        else
          case currentRow of
                [] -> cartesianDataRec
                        mergedData
                        []
                        ( i + 1 )
                        0
                        myDict
                cell :: cells ->
                    cartesianDataRec
                        mergedData
                        cells
                        i
                        ( j + 1 )
                        (Dict.insert (i, j) cell myDict)


cartesianData: List ( List Entry )-> Dict ( Int, Int ) Entry
cartesianData mergedData =
    cartesianDataRec mergedData [] 0 0 Dict.empty


mergeData: List Component -> ( List ( List Entry ), List String, List String )
mergeData components =
    let dates = List.sort
                <| Set.toList
                    <| List.foldl
                            Set.union
                            Set.empty
                            <| List.map
                                (\ c ->  Set.fromList ( onlyActiveKeys c.data )
                                )
                                components
        columns = List.map
                    ( \ c -> c.name )
                    components

    in
        ( List.map
            ( builRowBasic components )
            dates
        , dates
        , columns
        )


builRowBasic: List Component -> String -> List  Entry
builRowBasic components date =
        List.map
            ( getEntry date )
            <| List.map
                (\ c -> ( c.name , c.data )
                )
                components


applyDiff: Model -> Model
applyDiff model =
    setupFill { model | diff = currentDiff model model.coordData }


currentDiff: Model -> Dict ( Int, Int ) Entry -> Dict ( Int, Int ) Entry
currentDiff model coordData =
    Dict.map
    (\ _ e -> { e | edition = ( linearCorrection model e.edition )})
        <| Dict.filter
            (\ _ e -> case e.edition of
                        Edition _ -> True
                        Deletion -> True
                        _ -> False
            )
            coordData


pasteRectangle: Dict ( Int, Int ) Entry -> Dict ( Int, Int ) String -> ( Int, Int ) -> Dict ( Int, Int ) Entry
pasteRectangle base patch ( cornerRow, cornerCol ) =
    let translatedPatch = Dict.fromList
                            <| List.map
                                (\ (( i, j ), v ) ->
                                    (( i + cornerRow, j + cornerCol ), v ))
                                ( Dict.toList patch )
    in
        Dict.merge
            ( \_ _ dict -> dict )
            ( \ position entryBase sPatch dict ->
                if entryBase.editable
                    then
                        Dict.insert
                            position
                            (patchEntry entryBase sPatch)
                            dict
                    else dict
            )
            ( \_ _ dict -> dict )
            base
            translatedPatch
            base


patchEntry: Entry -> String -> Entry
patchEntry entry s =
    { entry | raw = Just s
            , edition = parseInput s
    }


patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let allSeries = [ likeComp model ] ++ model.components
    in
    Cmd.batch <|  List.map
                        ( saveComponent
                            model.baseurl
                            model.horizon.timeZone
                            model.diff
                        )
                        allSeries


saveComponent: String -> String -> Dict ( Int, Int ) Entry -> Component -> Cmd Msg
saveComponent baseUrl tzone series component =
    let name = component.name
        tzaware = component.tzaware
        patch = filterAndConvert series name
    in
        if Dict.isEmpty patch
            then Cmd.none
        else
        Http.request
            { method = "PATCH"
            , body = Http.jsonBody <| JE.object
                     ([ ("name", JE.string name )
                     , ("author" , JE.string "webui" )
                     , ("tzaware", JE.bool tzaware )
                     , ("series", encodeEditedData patch )
                     , ("supervision", JE.bool True )
                     , ("keepnans", JE.bool True)
                     ] ++ if tzaware
                            then [( "tzone", JE.string tzone )]
                            else []
                     )
            , headers = [ ]
            , timeout = Nothing
            , tracker = Nothing
            , url = UB.crossOrigin baseUrl
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


filterAndConvert: Dict ( Int, Int ) Entry -> String -> Dict String ( Maybe Float )
filterAndConvert editedData name =
    Dict.fromList
        <| List.concat
        <| Dict.values
            <| Dict.map
                (\ _ e -> case e.edition of
                            NoEdition -> []
                            Error _ -> []
                            Deletion -> [(e.indexRow, Nothing)]
                            Edition val -> [(e.indexRow, Just val)]
                )
                <| Dict.filter
                    ( \ _ e  -> e.indexCol == name )
                    editedData


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
        (( ( maybeRoundForm model )
        ++ if not ( isSingle model )
            then [ buttonShowDiff model ]
            else [ ] )
            ++ [ buttonFillAll model ]
       )


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
        Creation _ -> []
        _ ->
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


disableSave: Model -> Bool
disableSave model =
    case model.mode of
        Creation _ -> model.creation.nameStatus == Invalid
                      || model.creation.nameStatus == Missing
        _ -> False


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


buttonFillAll: Model -> H.Html Msg
buttonFillAll model =
    H.button
        [ HA.class "bluebutton top-button custom-button button-fill-all"
        , HE.onClick FillAll
        , HA.title "Forward-fill on all selection"
        , HA.disabled ( model.lastValids == [] )
        ]
        [ H.text "Fill All ↓" ]



viewRelevantTable: Model -> H.Html Msg
viewRelevantTable model =
    case model.mode of
        Existing I.Primary -> viewValueTable model
        Existing I.Formula -> viewValueTable model
        Creation Form -> H.table
                            [ HA.class "creation-form" ]
                            ( [ nameForm model ] ++ ( creationForm model ) )

        Creation Edit -> H.div
                            [ ]
                            [ H.table
                                [ HA.class "creation-form" ]
                                [ nameForm model ]
                            , viewValueTable model ]


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
    H.tr
        [ ]
        [ H.td
            [ ]
            [ H.label
                [ HA.for "creation-name" ]
                [ H.text "Name " ]
            ]
        , H.td
            [ ]
            [ H.input
                [ HA.id "creation-name"
                , HA.class ( className model.creation.nameStatus )
                , HA.name "name"
                , HA.autocomplete False
                , HE.onClick UnFocus
                , HE.onInput ( \s -> Create ( Name s ) )
                , HA.value model.creation.name
                ]
                []
            ]
        ]

creationForm: Model -> List ( H.Html Msg )
creationForm model =
        [ H.tr
            []
            [ H.td
                []
                [ H.label
                    [HA.for "creation-from"]
                    [H.text "From "]
                ]
            , H.td
                []
                [ H.input
                    [ HA.type_ "date"
                    , HA.id "creation-from"
                    , HA.class "input-date"
                    , HA.class  ( checkMandatory model.creation.from )
                    , HA.name "from"
                    , HE.onInput ( \s -> Create ( From s ) )
                    , HA.value model.creation.from
                    ]
                    []
                ]
            ]
        , H.tr
            []
            [ H.td
                [ ]
                [ H.label
                    [HA.for "creation-to"]
                    [H.text "To "]
                ]
            , H.td
                [ ]
                [ H.input
                    [ HA.type_ "date"
                    , HA.id "creation-to"
                    , HA.class "input-date"
                    , HA.class  ( checkMandatory model.creation.to )
                    , HA.name "to"
                    , HE.onInput ( \s -> Create ( To s ) )
                    , HA.value model.creation.to
                    ]
                    []
                ]
            ]
        , H.tr
            [ ]
            [ H.td
                [ ]
                [ H.label
                    [ HA.for "creation-tz" ]
                    [ H.text "Timezone " ]
                ]
            , H.td
                [ ]
                [ tzoneDropdown
                    model.horizon.timezones
                    model.creation.tz
                    model.horizon.timeZone
                ]
            ]
        , H.tr
            []
            [ H.td
                [ ]
                [ H.label
                    [ HA.for "creation-multiplier"]
                    [ H.text "Frequency " ]
                ]
            , H.td
                [ ]
                [ H.input
                    [ HA.id "creation-multiplier"
                    , HA.type_ "number"
                    , HA.min "1"
                    , HA.step "1"
                    , HA.placeholder "1"
                    , HA.name "multiplier"
                    , HE.onInput ( \s -> Create ( FreqMultiply s ) )
                    , HA.value ( case model.creation.freq.multiplier of
                                    Nothing -> ""
                                    Just n -> String.fromInt n
                               )
                    ]
                    []
                , offsetDropdown model
                ]
            ]
        , H.tr
            []
            [ H.td
                []
                [ H.label
                    [ HA.for "creation-value"]
                    [ H.text "Value " ]
                ]
            , H.td
                []
                [ H.input
                    [ HA.id "creation-value"
                    , HA.type_ "number"
                    , HA.name "value"
                    , HA.placeholder "NaN"
                    , HE.onInput ( \s -> Create ( Value s ) )
                    , HA.value ( case model.creation.value of
                                    Nothing -> ""
                                    Just f -> String.fromFloat f
                               )
                    ]
                    []
                ]
            ]
        , H.tr
            []
            [ H.td [] []
            , H.td
                []
                [ H.button
                    [ HE.onClick ( Create Preview )
                    , HA.disabled ( not model.creation.mandatoryValid )
                    , HA.class "bluebutton custom-button"
                    ]
                    [ H.text "Preview" ]
                ]
            ]
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


renderTimeZone : TzSelector -> String -> String -> H.Html Msg
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


offsetDropdown: Model -> H.Html Msg
offsetDropdown model =
    let
        decodeValue : String -> JD.Decoder Msg
        decodeValue offset =
            JD.succeed (Create (FreqOffset offset))

    in
    H.select
        [ HE.on "change" (JD.andThen decodeValue HE.targetValue)
        , HA.id "creation-offset"
        , HA.name "offset"
        ]
        ( List.map (renderOffset model.creation.freq.offset ) model.creation.offsets )


renderOffset : String -> String ->H.Html Msg
renderOffset selected offset =
        H.option
            [ HA.value offset
            , HA.selected ( offset == selected )
            ]
            [ H.text offset ]

-- Main table

viewValueTable: Model -> H.Html Msg
viewValueTable model =
    let nbPoints = List.length ( datesValue model )
    in
    case statePoints nbPoints model.forceDraw
    of
        NoPoint
            ->  H.div [][ ]
        TooMuchPoints nb ->
            H.div
                [ ]
                [ msgTooManyPointsWithButton nb ]
        Drawable ->
            H.div
                []
                (if isSingle model
                then
                   [ H.div
                        [ HA.class "wrapper-primary" ]
                        [ buildTable
                            model
                            ( headerShowValue model )
                            ( getIndexedDates model )
                        , strap
                        , forCurrentDiff model
                        ]
                    ]
                else
                    [ forCurrentDiff model
                    , buildTable
                        model
                        ( headerShowValue model )
                        ( getIndexedDates model )
                    ]
                )

strap =
    H.div
        [ HA.class "separator-primary" ]
        []


forCurrentDiff: Model -> H.Html Msg
forCurrentDiff model =
    if not model.showDiff &&  not ( isSingle model )
    then H.div [ HA.id "placeholder-save-table" ] []
    else
        H.div
            [ HA.class "save-table" ]
            [ H.div
                  [ HA.class "for-current-diff"]
                  ( divLinearCorrection model model.diff
                  ++ saveButtons model model.diff
                  )
                , buildDiffTable model
            ]


getIndexedDates: Model -> Dict Int ( H.Html Msg )
getIndexedDates model =
    Dict.fromList
        <|List.indexedMap
            Tuple.pair
            <| List.map
                ( \d ->  H.th
                            [HA.class "show-table-dates"]
                            [ H.text d ]
                )
                ( datesValue model )


getBounds:  Dict ( Int, Int ) a -> ( ( Int, Int ),  ( Int, Int ))
getBounds cartDict=
    let coords = Dict.keys cartDict
        rows = List.map Tuple.first coords
        cols = List.map Tuple.second coords
        minRow = Maybe.withDefault 0 <| List.minimum rows
        maxRow = Maybe.withDefault 0 <| List.maximum rows
        minCol = Maybe.withDefault 0 <| List.minimum cols
        maxCol = Maybe.withDefault 0 <| List.maximum cols
    in
        ( ( minRow, maxRow ), ( minCol, maxCol ))


buildCoord: Model -> Model
buildCoord model =
    let nbPoints = List.length <| datesValue model
    in
    case statePoints nbPoints model.forceDraw of
         NoPoint -> { model | coordData = Dict.empty }
         TooMuchPoints _ -> { model | coordData = Dict.empty }
         Drawable ->
            let allSeries = [ likeComp model ] ++ model.components
                ( dataAsRows, dates, columns ) = mergeData allSeries
            in
            setupFill
                { model | coordData = ( cartesianData dataAsRows)
                        , dates = dates
                        , columns = columns
                }


updateCoordData: Model -> (Int, Int) -> Maybe String -> Edited -> Model
updateCoordData model position raw edition =
    let previous = Maybe.withDefault
                    emptyEntry
                    ( Dict.get position model.coordData )
    in
        if not previous.editable
            then model
            else
    let
        newCoord = Dict.insert
                    position
                    { previous | raw = raw
                               , edition = edition
                    }
                    model.coordData
    in
        { model | coordData = newCoord }


getIndexes: Dict ( Int, Int ) Entry -> ( ( Int, Int ),  ( Int, Int ) ) -> ( Dict Int String, Dict Int String )
getIndexes coordData bounds =
    let (( minRow, maxRow ), ( minCol, maxCol )) = bounds
        dates = List.map
                (\ iRow -> ( iRow , ( findEntry (iRow, minCol) ( 0, 1 ) coordData bounds ).indexRow ) )
                ( List.range minRow maxRow )
        columns = List.map
                (\ iCol -> ( iCol, ( findEntry (minRow, iCol) ( 1, 0 ) coordData bounds).indexCol ) )
                ( List.range minCol maxCol )
    in
        ( Dict.fromList dates, Dict.fromList columns )


relevantIndexes: Dict ( Int, Int ) a -> ( List Int, List Int )
relevantIndexes coordData =
    let keys = Dict.keys coordData
        rows = Set.fromList ( List.map Tuple.first keys )
        cols = Set.fromList ( List.map Tuple.second keys )
    in
        ( Set.toList rows,
          Set.toList cols
        )


findEntry: ( Int, Int ) ->  ( Int, Int ) -> Dict ( Int, Int ) Entry -> ( ( Int, Int ),  ( Int, Int ) ) -> Entry
findEntry ( iRow,  iCol ) increment coordData bounds =
    let (( _, maxRow ), ( _, maxCol )) = bounds
    in
    if iRow > maxRow || iCol > maxCol
        then emptyEntry
        else
    case Dict.get (iRow, iCol) coordData of
        Just e -> e
        Nothing -> findEntry ( addP (iRow, iCol) increment ) increment coordData bounds


buttonShowDiff: Model -> H.Html Msg
buttonShowDiff model =
    let msg = if model.showDiff
                then "Hide diff"
                else "Show diff"
    in
    if isSingle model
    then
        H.div [ HA.id "placeholder-btn-show-diff" ] []
    else
        H.button
            [ HA.class "bluebutton top-button custom-button show-diff"
            , HA.disabled ( Dict.isEmpty model.diff )
            , HE.onClick ShowDiff
            ]
            [ H.text msg ]


buildDiffTable: Model -> H.Html Msg
buildDiffTable model =
     let diff = model.diff
         (( minRow, maxRow ), ( minCol, maxCol )) = getBounds diff
         ( dates, columns ) = getIndexes diff ( ( minRow, maxRow ), ( minCol, maxCol ))
         ( relevantRows, relevantCols) = relevantIndexes diff
    in
        H.table
        [ HA.class "diff-table"]
        [ H.thead
            []
            [ H.tr
                []
                ([ H.th [HA.class "diff-dates" ] [] ] ++ List.map
                    ( \ iCol ->  H.th
                                    []
                                    [ H.text <|
                                            Maybe.withDefault
                                                ""
                                                ( Dict.get iCol columns )
                                    ]
                    )
                    relevantCols
                )
            ]
        , H.tbody
            []
            ( List.map
                ( buildDiffRow model diff relevantCols dates )
                relevantRows )
        ]


buildDiffRow model diff relevantCols dates iRow  =
    H.tr
        []
        ( [ H.th
            [ HA.class "diff-dates" ]
            [ H.text <|
                Maybe.withDefault
                    ""
                    ( Dict.get iRow dates )
            ]
            ] ++  List.map
                ( buildDiffCell model diff iRow )
                relevantCols
        )


buildDiffCell model diff iRow iCol =
    let entry = case Dict.get ( iRow, iCol ) diff of
                    Nothing -> emptyEntry
                    Just content -> content
    in
        H.td [] [ H.text <| getValue entry ]


buildTable: Model -> List ( H.Html Msg ) -> Dict Int ( H.Html Msg ) -> H.Html Msg
buildTable model header rowDict =
    let cartDict = model.coordData
        ( ( minRow, maxRow ), ( minCol, maxCol )) = getBounds cartDict
    in
        H.table
        [ HA.class "edit-table"
        , HA.class "unselectable"]
        [ H.thead
            []
            header
        , H.tbody
            []
            ( List.map
                ( buildCartRow model cartDict rowDict minCol maxCol )
                ( List.range minRow maxRow ) )
        ]


buildCartRow: Model -> Dict ( Int, Int ) Entry ->  Dict Int ( H.Html Msg ) -> Int -> Int -> Int -> H.Html Msg
buildCartRow model cartDict rowDict minCol maxCol iRow =
    let indexRow = Dict.get iRow rowDict
        restRow =  List.map
                    ( buildCell model cartDict iRow )
                    ( List.range minCol maxCol )
    in
    case indexRow of
        Just index ->
            H.tr
                [ HA.class "row-edit" ]
                ( [ index ] ++ restRow  )
        Nothing ->
             H.tr
                []
                restRow


buildCell: Model -> Dict ( Int, Int ) Entry -> Int -> Int -> H.Html Msg
buildCell model cartDict iRow iCol =
    let entry = case Dict.get ( iRow, iCol ) cartDict of
                    Nothing -> emptyEntry
                    Just content -> content
        statusClass = cellStyle entry
        value = showValue entry
        valueCropped = printValue model.roundValues value
        focused = ( iRow,  iCol ) == Maybe.withDefault (-1, -1 ) model.focus
        selected = case model.selection of
                    Nothing -> False
                    Just box -> keyInSelection box ( iRow, iCol )
        fillUnder =  List.member ( iRow, iCol )  model.lastValids
        debug = model.horizon.debug
        active = case model.currentInput of
                    Nothing -> False
                    Just position -> (iRow, iCol) == position
   in
    H.td
        ([ if entry.editable
            then HA.class "editable"
            else HA.class "non-editable"
        , if focused
            then HA.class "focused"
            else HA.class ""
        , if selected
            then HA.class "selected"
            else HA.class ""
        , if fillUnder
            then HA.class "fill-under"
            else HA.class "plain"
        , if active
            then HA.class "active"
            else HA.class ""
        , HE.onClick ( ClickCell iRow iCol )
        , HE.onDoubleClick (ActivateCell iRow iCol )
        , HE.onMouseDown ( Drag ( On ( iRow, iCol ) ))
        ] ++  if model.holding.mouse
                then [ HE.onMouseEnter ( SelectRow ( iRow, iCol ) )
                     , HE.onMouseLeave ( SelectRow ( iRow, iCol ) )
                     ]
                else []
          ++ debugAttributes debug entry
        )
        [ H.node
            "batch-copy"
            [ HA.attribute "index" (idEntry (iRow, iCol) )
            , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
            ]
            ( ( contextualInput
                ( iRow, iCol )
                statusClass
                value
                valueCropped
                ( entry.editable && active )
            )
              ++ ( fillButton ( iRow, iCol ) fillUnder )
            )
        ]


contextualInput ( iRow, iCol ) statusClass value valueCropped editable =
    if editable
        then
            [ H.input
                    [ HA.id (idEntry (iRow, iCol) )
                    , HA.class statusClass
                    , HA.value value
                    , HA.autocomplete False
                    , HE.onInput ( InputChanged iRow iCol )
                     ]
                    [ ]
            ]
        else
            [ H.input
                    [ HA.id (idEntry (iRow, iCol) )
                    , HA.class statusClass
                    , HA.class "unselectable"
                    , HA.value valueCropped
                    , HA.readonly True
                    ]
                    [ ]
            ]


getEntry: String ->  ( String , Series ) -> Entry
getEntry date ( name, series ) =
    let defaultEntry = { raw = Nothing
                       , value = Nothing
                       , edition = NoEdition
                       , editable = False
                       , override = False
                       , indexRow = date
                       , indexCol = name
                       }
    in
    case series of
        Naked ts ->
            case ts.zoomTs of
                Just zoomTs ->
                    case Dict.get date zoomTs of
                        Just value ->  { defaultEntry | value = value
                                                          , raw = Maybe.andMap
                                                                    value
                                                                    (Just String.fromFloat)
                                           }
                        Nothing -> defaultEntry
                Nothing ->
                    case Dict.get date ts.initialTs of
                        Just value ->  { defaultEntry | value = value
                                                          , raw = Maybe.andMap
                                                                    value
                                                                    (Just String.fromFloat)
                                            }
                        Nothing -> defaultEntry
        ToEdit ts ->
            case ts.zoomTs of
                Just zoomTs ->
                        let entry = Maybe.withDefault emptyEntry ( Dict.get date zoomTs )
                        in
                            { defaultEntry | value = entry.value
                                            , edition = entry.edition
                                            , raw = Maybe.andMap
                                                        entry.value
                                                        (Just String.fromFloat)
                                            , override = entry.override
                                            , editable = True
                              }
                Nothing ->
                        let entry = Maybe.withDefault emptyEntry ( Dict.get date ts.initialTs )
                        in
                            { defaultEntry | value = entry.value
                                           , edition = entry.edition
                                           , override = entry.override
                                           , raw = Maybe.andMap
                                                        entry.value
                                                        (Just String.fromFloat)
                                           , editable = True
                               }


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
    let allDates = onlyActiveKeys comp.data
    in
        Set.fromList allDates


headerShowValue: Model -> List ( H.Html Msg )
headerShowValue model =
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
                    [ ]
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
        ( case comp.cType of
            Auto ->
                [ H.p [] [ H.text comp.name ]]
            _ ->
                [ H.a
                    [ HA.href ( UB.crossOrigin model.baseurl
                                    [ "tseditor" ]
                                    ( queryNav model comp.name ))]
                    [ H.text comp.name ]]
        )


buildFormater: Maybe Int -> String
buildFormater maybeRounds =
    case maybeRounds of
        Nothing -> "n"
        Just round -> ",." ++ String.fromInt round ++ "f"


printValue: Maybe Int -> String -> String
printValue round value =
    case String.toFloat value of
        Nothing -> value
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
          [ x ] ->  ( restoreSign negative )
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


restoreSign: Bool -> String -> String
restoreSign negative number =
    if negative
        then String.concat [ "-",  number ]
        else number


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



diffToFloat: List Entry -> List ( Maybe Float )
diffToFloat entries =
    List.map
        entryToFloat
        entries


entryToFloat: Entry -> Maybe Float
entryToFloat entry =
    case entry.edition of
        NoEdition -> Nothing
        Error _ -> Nothing
        Deletion -> Nothing
        Edition edit -> Just edit


saveButtons: Model -> Dict ( Int, Int ) a -> List (H.Html Msg)
saveButtons model diff =
    if Dict.isEmpty diff
        then [ ]
        else
            [ H.div
                [ HA.class "save-buttons" ]
                [ H.button
                  [ HA.class "yellowbutton custom-button save-button"
                  , HA.attribute "type" "button"
                  , HE.onClick CancelEdition
                  , HA.disabled (model.horizon.plotStatus == Loading)
                  ]
                  [ H.text "Cancel" ]
                , H.button
                  [ HA.class "greenbutton custom-button save-button"
                  , HA.attribute "type" "button"
                  , HE.onClick SaveEditedData
                  , HA.disabled ( disableSave model )
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


divLinearCorrection: Model -> Dict ( Int, Int ) a -> List (H.Html Msg)
divLinearCorrection model filtredDict =
    case ( Dict.isEmpty filtredDict ) of
        False -> [
            H.div
                [ HA.class "linear-correction"
                , HE.onClick UnFocus
                ]
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


getValue : Entry -> String
getValue entry =
     case entry.edition of
                Edition v -> String.fromFloat v
                Error s -> s
                Deletion -> "-"
                NoEdition ->
                    Maybe.unwrap
                        ""
                        String.fromFloat
                        entry.value

showValue: Entry -> String
showValue entry =
    case entry.raw of
        Nothing-> getValue entry
        Just stuff -> stuff


cellStyle: Entry -> String
cellStyle entry =
        case entry.edition of
            Edition _ -> "editing"
            Deletion -> "editing"
            Error _ -> "invalid"
            NoEdition ->
                if entry.override
                 then "override"
                 else if Maybe.isNothing entry.value
                      then "nan"
                      else ""


fillButton: ( Int, Int ) -> Bool -> List ( H.Html Msg )
fillButton position fillUnder =
    case fillUnder of
        True -> [ H.button
                    [ HA.title "Fill"
                    , HA.class "fill-button"
                    , HE.onClick ( FillNas position )]
                    [ H.text "↓" ]
                ]
        False -> []


isEmpty: Model -> Bool
isEmpty model =
    List.length ( onlyActiveKeys model.series ) == 0

displayCoord: ( Int, Int ) -> String
displayCoord (i, j) =
    "(" ++ (String.fromInt i) ++ " , " ++ (String.fromInt j) ++ ")"

displayBox: Maybe Box -> String
displayBox selection =
    case selection of
        Nothing -> "Nothing"
        Just box ->  ( displayCoord ( box.t, box.b ))
                     ++ ( displayCoord ( box.l, box.r ))


debugAttributes: Bool -> Entry -> List ( Attribute msg )
debugAttributes debug entry =
    if not debug
        then [ ]
        else
            [ HA.attribute "raw" ( displayRaw entry.raw )
            , HA.attribute "value" ( displayValue entry.value )
            , HA.attribute "edition" ( displayEdition entry.edition )
            , HA.attribute "override"  <| if entry.override then "True" else "False"
            , HA.attribute "indexRow" entry.indexRow
            , HA.attribute "indexCol" entry.indexCol
            ]


displayRaw: Maybe String -> String
displayRaw raw =
    case raw of
        Nothing -> "Nothing"
        Just r -> r

displayValue: Maybe Float -> String
displayValue value =
    case value of
        Nothing -> "Nothing"
        Just v -> String.fromFloat v

displayEdition: Edited -> String
displayEdition edited =
    case edited of
        Edition value -> "Edition-" ++ String.fromFloat value
        NoEdition -> "NoEdition"
        Deletion -> "Deletion"
        Error e -> "Error-" ++ e

splitRaw: String -> String
splitRaw raw =
    String.join
        "-"
        (List.map
            String.fromInt
            ( List.map
                Char.toCode
                ( String.toList raw )
            )
        )


debugView: Model -> H.Html Msg
debugView model =
    let option =  { githubFlavored = Just { tables = True, breaks = False }
                    , defaultHighlighting = Nothing
                    , sanitize = False -- The important part.
                    , smartypants = False
                    }
    in
    H.div
        []
        ( if model.horizon.debug
            then
                [ H.pre []
                ( [ H.text " debug active "
                , H.br [] []
                , H.text ( "Raw pasted : " ++ splitRaw model.rawPasted )
                , ( Markdown.toHtmlWith option [] model.rawPasted )
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
                , H.text ( "correction : "
                         ++  ( displayRaw model.slope )
                         ++ " / "
                         ++ ( displayRaw model.intercept )
                         )
                , H.br [] []
                , H.text  ( ", Key Pressed: " ++ model.keyName)
                , H.br [] []
                , H.text (", Selectionbox: " ++ displayBox model.selection )
                , H.text  ( ", Focus : " ++ case model.focus of
                                                Nothing -> "Nothing"
                                                Just focus -> displayCoord focus
                                )

                , H.text  ( ", FirstShift: " ++ case model.firstShift of
                                Nothing -> "Nothing"
                                Just first -> displayCoord first
                        )
                , H.text  ( ", current Active: " ++ case model.currentInput of
                                Nothing -> "Nothing"
                                Just active -> displayCoord active
                        )
                ] ++ ( List.map
                            (\ i -> H.text (", Na to fill at: " ++ ( displayCoord i )))
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
           then H.text """No data in this interval: select another one."""
           else H.text ""

msgDoesNotExist =
     H.div
        []
        [ H.text "Series does not exists. Check your url."]


plotNode: Model -> H.Html Msg
plotNode model =
    let dates = onlyActiveKeys model.series
        values = Dict.values ( onlyActiveValues model.series )
        diff = Dict.values ( currentDiff model model.coordData )
        editionTrace = case model.mode of
                        Existing I.Formula -> []
                        _ -> [ scatterplot
                                    "edition"
                                    ( List.map
                                        (\ e -> e.indexRow)
                                        diff
                                    )
                                    ( diffToFloat diff )
                                    "markers"
                                    defaultTraceOptions
                             ]
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
                    ( [ scatterplot
                        model.name
                        dates
                        values
                        ( if model.horizon.inferredFreq
                            then "lines+markers"
                            else "lines" )
                        defaultTraceOptions
                      ] ++ editionTrace
                    )
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
            [ HA.class "header-with-horizon"
            , HE.onClick UnFocus
            ]
            [ H.div [ HA.class "action-container" ]
                  <| I.viewactionwidgets
                        model
                        (I.SeriesType model.seriestype)
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
                , case model.mode of
                    Existing _ -> if model.exist
                                    then H.div [ HA.id "plot" ] [ ]
                                    else msgDoesNotExist
                    _ -> H.div [ HA.id "plot" ] [ ]
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
        Existing _ -> M.getsysmetadata
                            model.baseurl
                            model.name
                            GotMetadata
                            "series"
        Creation _ -> Cmd.batch [ getCatalog model
                                , getOffsets model
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
                    , mode = if input.name == ""
                                then Creation Form
                                else Existing I.Primary
                    , meta = Dict.empty
                    , exist = False
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
                    , showDiff = False
                    , intercept = Nothing
                    , slope = Nothing
                    , insertion_dates = Array.empty
                    , processedPasted = [ ]
                    , rawPasted = ""
                    , selection = Nothing
                    , focus = Nothing
                    , firstShift = Nothing
                    , firstSelected = Nothing
                    , currentInput = Just (1, 1)
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
                    , coordData = Dict.empty
                    , dates = []
                    , columns = []
                    , diff = Dict.empty
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

