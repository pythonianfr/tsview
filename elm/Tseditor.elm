port module Tseditor exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Dom exposing (Error(..))
import Browser.Events exposing
    ( onKeyDown
    , onKeyUp
    )
import Browser.Navigation
import Dict exposing (Dict)
import Date
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , ZoomFromPlotly
    , extendHorizonFromData
    , extractDates
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
import Json.Encode as JE
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Metadata as M
import OrderedDict as OD
import Plotter exposing
    ( Axis
    , Trace
    , defaultDateAxis
    , defaultLayoutOptions
    , defaultTraceOptions
    , defaultConfigOptions
    , getdata
    , scatterplot
    , serializedPlotArgs
    )
import Process as P
import Round
import Set exposing (Set)
import StatInfos as ModuleStatInfos
import StatInfos exposing
    ( Msg(..)
    , StatInfos
    , TypeStat
    , StatusFreq
    , emptyStat
    , getStatistics
    , formatNumber
    , updateFirstLast
    , viewStatTable
    )
import Task as T
import Url.Builder as UB
import Util as U


port copyToClipboard : String -> Cmd msg
port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg
port panActive : (Bool -> msg) -> Sub msg
port copySignal: (Bool -> msg) -> Sub msg
port saveLocal : LocalStorage -> Cmd msg
port loadLocal : (String -> msg) -> Sub msg


keyDecoder : Action -> JD.Decoder Msg
keyDecoder action =
    JD.map ( toKey action ) (JD.field "key" JD.string)


toKey : Action -> String -> Msg
toKey action keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            Typing action char
        _ ->
            ActionControl ( keyToType action keyValue )


keyToType:  Action -> String -> ControlKey
keyToType action keyValue =
    case keyValue of
        "Escape" -> Escape action
        "Delete" -> Delete action
        "Control" -> Control action
        "Meta" -> Meta action
        "Shift" -> Shift action
        "ArrowUp" -> ArrowUp action
        "ArrowDown" -> ArrowDown action
        "ArrowLeft" -> ArrowLeft action
        "ArrowRight" -> ArrowRight action
        "PageDown" -> PageDown action
        "PageUp" -> PageUp action
        "Enter" -> Enter action
        _ ->  Other keyValue action


type Action
    = Up
    | Down


type ControlKey
    = Escape Action
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


naiveTag = "Naive"


type alias Model =
    { baseurl : String
    , name : String
    , basket: String
    , mode: EditionMode
    , meta : M.Metadata
    , exist: Bool
    , source : String
    , seriestype : I.SeriesType
    , tzaware: Bool
    , string: Bool
    , horizon : HorizonModel
    , creation: CreationModel
    , newBatch : Bool
    -- data
    , insertion_dates : Array String
    , series : Series
    , statistics: StatInfos
    , roundStat: Int
    , statVisibility: Bool
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
    , mousePosition: Maybe ( Int, Int )
    , focus : Maybe ( Int, Int )
    , firstShift: Maybe ( Int, Int )
    , firstSelected : Maybe ( Int, Int )
    , lastValids: List ( Int, Int )
    , currentInput : Maybe ( Int, Int )
    , nameVisbility : Visibility
    , slope: Maybe String
    , intercept: Maybe String
    -- keyboard/mouse
    , keyName: String
    , holding: Holding
    -- show-values for formula
    , expand : Bool
    , directComponents : List Component
    , terminalComponents : List Component
    , coordData: Dict ( Int, Int ) Stuff
    , diff : Dict ( Int, Int ) Entry
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotValueData (Result Http.Error String)
    | GotComponents Bool (Result Http.Error String)
    | GotComponentData CType String Bool (Result Http.Error String)
    | GotGenerated PreviewType (Result Http.Error String)
    | GotBasket (Result Http.Error String)
    | GotMetadata (Result Http.Error String) -- first command fired
    | GotSource (Result Http.Error String)
    | GotCatalog (Result Http.Error String)
    | GotOffsets (Result Http.Error String)
    | Horizon ModuleHorizon.Msg
    | DateNow Date.Date
    | Create CreationOptions
    | Back
    | SwitchForceDraw
    | SwitchBatch
    | StatVisible Bool
    | StatInfos ModuleStatInfos.Msg
    | ShowDiff
    | Expand Bool
    | AllVisible Bool
    | Visible Int
    | InputChanged Int Int String
    | SaveEditedData
    | SaveAnyway
    | Saved (Result Http.Error String)
    | CancelEdition
    | Correction Parameter
    | Paste PasteType
    | MousePosition ( Int, Int )
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
    | Focus (Result Browser.Dom.Error ())
    | FillNas ( Int, Int)
    | FillAll
    | FromLocal String
    | NewRound ActionRound
    | InsertionDates (Result Http.Error String)
    | GetLastInsertionDates (Result Http.Error String)
    | GetLastEditedData (Result Http.Error String)
    | FromZoom ZoomFromPlotly
    | NewDragMode Bool


type ScalarType f s
    = Float f
    | String s

type Scalar = ScalarType Float String

type alias SeriesNaked = Dict String ( Maybe ( ScalarType Float String ))
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


type EditionMode
    = Creation CreationMode
    | Existing I.SeriesType
    | BasketMode


type CreationMode
    = Form
    | Edit


type CreationOptions
    = Name String
    | From String
    | To String
    | FreqOffset String
    | FreqMultiply String
    | Tz String
    | Value String
    | Preview PreviewType


type Visibility =
    Nope
    | Single Int
    | All


type PreviewType
    = FromScratch
    | Patch


type alias FreqType =
    { offset: String
    , multiplier: Maybe Int
    }


type alias CreationModel =
    { from: Maybe String
    , to: Maybe String
    , freq: FreqType
    , tz: TzSelector
    , value: Maybe String
    , name: Maybe String
    , nameStatus: NameStatus
    , mandatoryValid: Bool
    , catalog : Maybe ( List String )
    , offsets : List String
    }


type TzSelector
    = Unchanged
    | Naive
    | Selected String


type NameStatus
    = Valid
    | Invalid
    | Missing


initCreationModel: CreationModel
initCreationModel =
    { from = Nothing
    , to = Nothing
    , freq = { offset = "h"
             , multiplier = Nothing
             }
    , tz = Unchanged
    , value = Nothing
    , name = Nothing
    , nameStatus = Missing
    , mandatoryValid = True
    , catalog = Nothing
    , offsets = []
    }


convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


convertStat : ModuleStatInfos.Msg -> Msg
convertStat msg =
    StatInfos msg


type Parameter
    = Slope String
    | Intercept String


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


type ActionRound
    = Replace String
    | Remove
    | More
    | Less


type alias LocalStorage =
    { round : Maybe String }


maxPoints = 1000


msgTooManyPoints nbPoints =
    H.text
        <|  """ Too many points to display. ("""
            ++ String.fromInt nbPoints
            ++ """) Please select a smaller time
            frame or an area on the graph."""


type CopyType
    = CopyName
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

type alias BaseSupervisionString =
    { value : Maybe String
    , override : Bool
    }

type Stuff
    = DateRow String
    | Header ( String, CType )
    | Cell Entry


filterEntry: Dict ( Int, Int ) Stuff ->  Dict ( Int, Int ) Entry
filterEntry coordData =
    Dict.map
        (\ _ stuff -> case stuff of
                       Cell entry -> entry
                       DateRow _ -> emptyEntry
                       Header _ -> emptyEntry
        )
        <| Dict.filter
            (\  _ stuff -> case stuff of
                            Cell _ -> True
                            DateRow _ -> False
                            Header _ -> False
            )
            coordData


type alias Entry =
    { raw : Maybe String
    , value : Maybe ( ScalarType Float String )
    , edition : Edited
    , editable: Bool
    , override : Bool
    , indexRow: String
    , indexCol: String
    , fromBatch: Bool
    }


baseToEntry: BaseSupervision -> Entry
baseToEntry base =
    { raw = Maybe.map String.fromFloat base.value
    , value = Maybe.map Float base.value
    , edition = NoEdition
    , editable = True
    , override = base.override
    , indexRow = ""
    , indexCol = ""
    , fromBatch = False
    }

baseToEntryString: BaseSupervisionString -> Entry
baseToEntryString base =
     { raw = base.value
    , value = Maybe.map String base.value
    , edition = NoEdition
    , editable = True
    , override = base.override
    , indexRow = ""
    , indexCol = ""
    , fromBatch = False
    }

toScalar: Maybe Float -> Maybe ( ScalarType Float String )
toScalar mf =
    Maybe.map Float mf

toString: Maybe String -> Maybe ( ScalarType Float String )
toString mf =
    Maybe.map String mf

mapToFloat : SeriesNaked -> Dict String ( Maybe Float )
mapToFloat =
    Dict.map
        (\_ v -> toFloat v )

mapToString : SeriesNaked -> Dict String ( Maybe String )
mapToString =
    Dict.map
        (\_ v -> case v of
                    Nothing -> Nothing
                    Just scal ->
                        case scal of
                            Float _ -> Nothing
                            String s -> Just s
        )

toFloat: Maybe ( ScalarType Float String ) -> Maybe Float
toFloat ms =
    case ms of
        Nothing -> Nothing
        Just scal ->
            case scal of
                Float f -> Just f
                String _ -> Nothing

toRaw: ( ScalarType Float String ) -> String
toRaw scal =
    case scal of
        String s -> s
        Float f -> String.fromFloat f

emptyEntry : Entry
emptyEntry =
    { raw = Nothing
    , value = Nothing
    , edition = NoEdition
    , editable = False
    , override = False
    , indexRow = ""
    , indexCol = ""
    , fromBatch = False
    }


asEdited: Maybe ( ScalarType Float String ) -> Edited
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
                                        Just val ->
                                            case val of
                                                Float f ->
                                                    Just
                                                      <| String.fromFloat f
                                                String s -> Just s
                               , indexCol = name
                               , indexRow = k
                               , fromBatch = False
                               }
                             )
            )
            ( Dict.toList series )


nameSeries : Dict ( Int, Int ) Entry -> String -> Dict ( Int, Int ) Entry
nameSeries series name =
     Dict.map
        ( \ _ e ->  { e | indexCol = name } )
        series


isTzaware: M.Metadata -> Bool
isTzaware meta =
     case Dict.get "tzaware" meta of
         Just (M.MBool val) -> val
         _ -> False


isStr : M.Metadata -> Bool
isStr meta =
    case M.dget "value_type" meta of
        "object" -> True
        _ -> False


likeComp: Model -> List Component
likeComp model =
    case model.mode of
        BasketMode ->
            []
        _ ->
             [ Component
                model.name
                ( asCType model.seriestype )
                model.series
                model.tzaware
                CompLoaded
            ]



type Edited
    = Edition ( ScalarType Float String )
    | NoEdition
    | Deletion
    | Error String


-- would need source & last insertion date field
type alias Component =
    { name: String
    , cType: CType
    , data: Series
    , tzaware: Bool
    , status: CompStatus
    }


type CompStatus
    = CompEmpty
    | CompLoaded
    | CompError

type alias BasketItem =
    { name : String
    , imeta : Maybe M.Metadata
    , meta : Maybe M.Metadata
    , source : String
    , kind : String
    }


toComp: BasketItem -> Component
toComp basket =
    { name= basket.name
    , cType = if basket.kind == "primary" then Primary else Formula
    , data = emptySeries
    , tzaware = case basket.imeta of
                    Nothing -> True
                    Just imeta -> isTzaware imeta
    , status = CompEmpty
    }




asCType: I.SeriesType -> CType
asCType t =
    case t of
        I.Primary -> Primary
        I.Formula -> Formula


type CType
    = Primary
    | Formula
    | Auto


applyType: String -> CType
applyType strtype =
    case strtype of
        "auto" -> Auto
        "formula" -> Formula
        _  -> Primary


type DragMode
    = On ( Int, Int)
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

basketItemDecode : JD.Decoder BasketItem
basketItemDecode =
    JD.map5 BasketItem
        (JD.field "name" JD.string)
        (JD.field "imeta" (JD.succeed Nothing))
        (JD.field "meta" (JD.succeed Nothing))
        (JD.field "source" JD.string)
        (JD.field "kind" JD.string)

separatorReturn raw =
    if String.contains "\r\n" raw then Just "\r\n" -- windows
    else if String.contains "\n" raw then Just "\n" -- unix
    else Nothing


parsePasted : String -> Bool -> List ( List String )
parsePasted raw isString =
    let
        removedSpace = if isString
                        then
                            raw
                        else
                            String.replace " " "" raw
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


-- the index is of the form e/2/3
getPos: String -> ( Int, Int )
getPos s =
    case String.split "/" s of
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
        (JD.field "series" (JD.nullable JD.float))
        (JD.field "markers" JD.bool)

entryDecoderString : JD.Decoder BaseSupervisionString
entryDecoderString =
    JD.map2 BaseSupervisionString
        (JD.field "series" (JD.nullable JD.string))
        (JD.field "markers" JD.bool)


decodeToEdit : JD.Decoder (Dict String Entry)
decodeToEdit =
    ( JD.dict ( JD.map baseToEntry entryDecoder ))

decodeToEditString : JD.Decoder (Dict String Entry)
decodeToEditString =
    ( JD.dict ( JD.map baseToEntryString entryDecoderString ))

decodeNaked : JD.Decoder SeriesNaked
decodeNaked =
        (JD.dict (JD.map toScalar (JD.nullable JD.float)))

decodeNakedString : JD.Decoder SeriesNaked
decodeNakedString =
        (JD.dict (JD.map toString (JD.nullable JD.string)))

decodeSupervised: String ->  Result ( JD.Error, JD.Error ) SeriesToEdit
decodeSupervised raw =
    case JD.decodeString decodeToEdit raw of
        Ok ts -> Ok ts
        Err errFloat ->
            case JD.decodeString decodeToEditString raw of
                Ok ts -> Ok ts
                Err errString -> Err ( errFloat, errString )


decodeValues: String ->  Result ( JD.Error, JD.Error ) SeriesNaked
decodeValues raw =
    case JD.decodeString decodeNaked raw of
        Ok ts -> Ok ts
        Err errFloat ->
            case JD.decodeString decodeNakedString raw of
                Ok ts -> Ok ts
                Err errString -> Err ( errFloat, errString )


componentsDecoder: JD.Decoder (List Component)
componentsDecoder =
    JD.list (JD.map5 Component
                 ( JD.field "name" JD.string )
                 ( JD.map applyType ( JD.field "type" JD.string ))
                 ( JD.succeed emptySeries )
                 ( JD.field "tzaware" JD.bool )
                 ( JD.succeed CompEmpty )
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
        Creation Edit  -> True
        Existing I.Primary -> True
        _ -> False


getPoints: Model -> Cmd Msg
getPoints model =
    case model.mode of
        Existing I.Primary -> getSeries model GotEditData "supervision" GET model.name
        Existing I.Formula ->  getSeries model GotValueData "state" GET model.name
        Creation _ -> Cmd.none
        BasketMode -> Cmd.none


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
        , tzware = model.tzaware
        , tzone = model.horizon.timeZone
        , inferredFreq = model.horizon.inferredFreq
        , keepnans = True
        , apipoint = apipoint
        , exclude = "right"
        }


getOrPostData method query =
    case method of
        GET -> getdata query
        POST -> postData query


getComponents: Model -> Bool -> Cmd Msg
getComponents model expand =
    Http.get
        { url =
              UB.crossOrigin model.baseurl
              [ "formula-components" ]
              [ UB.string "name" model.name
              , UB.string "full" <| if expand then "true" else "false"
              ]
        , expect = Http.expectString ( GotComponents expand )
        }


getBasket : String -> String -> Cmd Msg
getBasket baseUrl name =
    Http.get
        { expect = Http.expectString GotBasket
        , url = UB.crossOrigin baseUrl
              [ "api", "series", "basket" ]
              [ UB.string "name" name ]
        }


getDataComponents: Model -> Bool -> Cmd Msg
getDataComponents model expand =
    Cmd.batch
        <| List.reverse
               ( List.map
                     ( getRelevantComponent model expand )
                     ( relevantComponents model )
               )


getRelevantComponent : Model -> Bool -> Component ->  Cmd Msg
getRelevantComponent model expand component  =
    case component.cType of
        Auto ->
            getSeries
                model
                ( GotComponentData Auto component.name expand )
                "eval_formula"
                POST
                component.name
        Primary ->
            getSeries
                model
                ( GotComponentData Primary component.name expand )
                "supervision"
                GET
                component.name
        Formula ->
            getSeries
                model
                ( GotComponentData Formula component.name expand )
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


getGeneratedTs: Model -> PreviewType -> Cmd Msg
getGeneratedTs model previewType=
    let
        baseparams =
            [ UB.string "from" ( Maybe.withDefault "" model.creation.from )
            , UB.string "to" ( Maybe.withDefault "" model.creation.to )
            , UB.string "freq" ( printFreq model.creation.freq )
            ]

        createparams1 =
            case model.creation.tz of
                Naive ->
                    [ ]
                Selected tz ->
                    [ UB.string "tz" tz ]
                Unchanged ->
                    if model.tzaware
                    then [ UB.string "tz" model.horizon.timeZone ]
                    else [ ]

        createparams2 =
            case model.creation.value of
                Nothing ->
                    [ ]
                Just value ->
                    [ UB.string "value" value]
    in
    Http.get
        { url =
              UB.crossOrigin model.baseurl
              [ "generate-ts" ]
              (baseparams ++ createparams1 ++ createparams2)
        , expect = Http.expectString ( GotGenerated previewType)
        }


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
        then JE.object [ ("text", JE.string formula) ]
        else JE.object [ ("text", JE.string formula)
                       , ("from_value_date", JE.string from)
                       , ("to_value_date", JE.string to)
                       ]


postData query =
    Http.post
        { url =
              UB.crossOrigin query.baseurl
              [ "api", "series", query.apipoint ]
              []
        , body =
            Http.jsonBody ( encodeBodyEvalFormula
                                query.name
                                query.fromdate
                                query.todate
                          )
        , expect = Http.expectString query.callback
        }


addError: Model -> String -> String -> Model
addError model tag error =
    U.adderror model (tag ++ " -> " ++ error)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)

    in
    case msg of
        GotEditData (Ok rawdata) ->
            case decodeSupervised rawdata of
                Ok indexedval ->
                    let
                        zoomTs = applyZoom model indexedval
                        series = ToEdit { initialTs = indexedval, zoomTs = zoomTs }
                        statistics = getStatistics
                                        model.statistics
                                        model.allowInferFreq
                                        ( onlyActiveValues series )
                    in
                        applyFocus
                            ( buildCoord
                                { model
                                    | series = series
                                    , statistics = statistics
                                    , horizon = updateHorizonFromData
                                                    model.horizon
                                                    (Dict.keys indexedval)
                                })
                        ( Just ( 0, 0 ) )

                Err (errFloat, errString) ->
                  U.nocmd ( addError
                                model
                                "got edit data decode"
                                (( JD.errorToString errFloat )
                                ++  ( JD.errorToString errString ))
                                )

        GotEditData (Err _) ->
            U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        GotValueData (Ok rawdata) ->
            case decodeValues rawdata of
                Ok val -> let zoomTs = applyZoom model val
                          in
                          ( ( buildCoord
                            { model | series = Naked { initialTs = val
                                                     , zoomTs = zoomTs }
                                    , horizon = updateHorizonFromData
                                                model.horizon
                                                (Dict.keys val)
                                    , statistics = getStatistics
                                                model.statistics
                                                model.allowInferFreq
                                                ( mapToFloat val )
                            }
                            )
                           , Cmd.batch [ getComponents model False
                                       , getComponents model True ]
                           )
                Err  (errFloat, errString) -> U.nocmd ( addError
                                        { model | horizon = setStatusPlot
                                                                model.horizon
                                                                Failure
                                        }
                                        "got value data decode"
                                        (( JD.errorToString errFloat ) ++ ( JD.errorToString errString ))
                                        )

        GotValueData (Err _) ->
            U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        GotComponents expand (Ok rawdata) ->
            case JD.decodeString componentsDecoder rawdata of
                Ok val ->
                    let newmodel =
                            if expand
                            then { model | terminalComponents = val}
                            else { model | directComponents = val}
                    in ( newmodel
                       , if not expand
                         then getDataComponents newmodel False
                         else Cmd.none
                       )
                Err err ->
                    U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}

        GotComponents _ (Err _) ->
            ( model, Cmd.none )

        GotBasket (Ok rawdata) ->
            case JD.decodeString
                    ( JD.list ( JD.map toComp basketItemDecode ))
                    rawdata of
                Ok names -> let newmodel = { model | directComponents = names}
                            in ( newmodel
                               , getDataComponents newmodel False
                                )
                Err err -> U.nocmd
                               { model | errors = model.errors ++ [JD.errorToString err]}

        GotBasket (Err _) -> ( model, Cmd.none )

        GotComponentData cType name expand (Ok rawdata) ->
            case cType of
                Primary ->
                    case decodeSupervised rawdata of
                        Ok indexedval ->
                            let zoomTs = applyZoom model indexedval
                                newCD = insertComponentData
                                            ( if expand
                                                then model.terminalComponents
                                                else model.directComponents
                                            )
                                            name
                                            CompLoaded
                                            ( Just ( ToEdit { initialTs = indexedval, zoomTs = zoomTs }))
                                newModel = if expand
                                            then { model | terminalComponents = newCD}
                                            else { model | directComponents = newCD}
                                status = if expand
                                            then multiStatus newModel.terminalComponents
                                            else multiStatus newModel.directComponents
                                newHorizon = extendHorizonFromData model.horizon indexedval
                            in
                                U.nocmd
                                    <| buildCoord { newModel |
                                                        horizon = setStatusPlot
                                                                    newHorizon
                                                                    status
                                                  }
                        Err (eS, eF) -> U.nocmd
                                    { model | errors = model.errors
                                                        ++ [JD.errorToString eS]
                                                        ++ [JD.errorToString eF]
                                    }
                _ ->
                    case JD.decodeString
                        decodeNaked
                        rawdata of
                    Ok val ->  let zoomTs = applyZoom model val
                                   newCD = insertComponentData
                                            ( if expand
                                                then model.terminalComponents
                                                else model.directComponents
                                            )
                                            name
                                            CompLoaded
                                            ( Just ( Naked { initialTs = val, zoomTs = zoomTs }))
                                   newModel = if expand
                                            then { model | terminalComponents = newCD}
                                            else { model | directComponents = newCD}
                                   status = if expand
                                            then multiStatus model.terminalComponents
                                            else multiStatus model.directComponents
                               in
                                   U.nocmd
                                    <| buildCoord { newModel |
                                                        horizon = setStatusPlot
                                                                    model.horizon
                                                                    status
                                                  }
                    Err err -> U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}

        GotComponentData cType name expand (Err _) ->
            let
                newCD = insertComponentData
                            ( if expand
                                then model.terminalComponents
                                else model.directComponents
                            )
                            name
                            CompError
                            Nothing
                newModel = if expand
                                then { model | terminalComponents = newCD}
                                else { model | directComponents = newCD}
            in
            U.nocmd { newModel | horizon = setStatusPlot model.horizon Failure }


        GotGenerated previewType ( Ok rawdata ) ->
             case JD.decodeString
                    decodeNaked
                    rawdata of
                Ok val ->
                    case previewType of
                        FromScratch ->
                            U.nocmd
                            <| applyDiff
                                <| buildCoord
                                    { model | series =
                                        ToEdit { initialTs = dressSeries
                                                                val
                                                                model.name
                                               , zoomTs = Nothing
                                               }
                                    , mode = Creation Edit
                                    }
                        Patch ->
                             U.nocmd
                            <| applyDiff
                                <| buildCoord
                                    { model | series =
                                         ToEdit { initialTs = patchCurrent
                                                                ( getEditionTs model.series )
                                                                val
                                                                model.name
                                                           , zoomTs = Nothing
                                                           }
                                    }

                Err err ->
                     U.nocmd { model | errors = model.errors ++ [JD.errorToString err]}

        GotGenerated previewType (Err err) ->
            U.nocmd { model | horizon = setStatusPlot model.horizon Failure }

        DateNow date ->
            let creation =
                    model.creation
                newCreation =
                    { creation
                        | from = Just (( Date.toIsoString date ) ++ "T00:00" )
                        , to = Just (( Date.toIsoString date ) ++ "T00:00" )
                    }
            in
            U.nocmd { model | creation = newCreation }

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
                                       , directComponents = cleanComponents model.directComponents
                                       , terminalComponents = cleanComponents model.terminalComponents
                              }
            in
            case hMsg of
                ModuleHorizon.Internal _ ->
                    default
                ModuleHorizon.Frame _ ->
                    ( { resetModel
                          | forceDraw = False
                          , firstSelected = Nothing
                      }
                    , moreCommands
                    )
                ModuleHorizon.FromLocalStorage _ ->
                    case model.mode of
                        BasketMode -> ( resetModel
                                       , Cmd.batch [ moreCommands
                                                   , commandStart resetModel
                                                   ]
                                       )
                        -- we want to fire the commands AFTER getting the metadata:
                        -- we store these commands in the model -.-
                        _ ->  ( { resetModel | initialCommands = moreCommands }
                              , Cmd.batch [ commandStart resetModel
                                          , T.perform DateNow Date.today
                                          ]
                              )

                ModuleHorizon.Fetch _ -> ( { resetModel | expand = False }
                                         , Cmd.batch ([ moreCommands ]
                                         ++ getRelevantData resetModel ))


        Create option ->
            let creation =
                    model.creation
                freq =
                    model.creation.freq
                newCreation =
                    case option of
                        From val -> { creation | from = Just val }
                        To val -> { creation | to = Just val }
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
                        Tz val ->
                            { creation | tz = if val == naiveTag
                                              then Naive
                                              else Selected val
                            }
                        Value val ->
                            { creation | value = Just val }
                        Name val ->
                            { creation
                                | name = Just val
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
                        _ ->
                            creation
                validatedCreation =
                    { newCreation
                        | mandatoryValid = newCreation.from /= Nothing &&
                          newCreation.to /= Nothing
                    }
                command =
                    case option of
                        Preview FromScratch -> getGeneratedTs model FromScratch
                        Preview Patch -> getGeneratedTs model Patch
                        _ -> Cmd.none
                tzawarness =
                    case validatedCreation.tz of
                        Naive -> False
                        Unchanged -> model.tzaware
                        _ -> True
            in
            ( { model
                  | creation = validatedCreation
                  , name = Maybe.withDefault model.name validatedCreation.name
                  , diff = nameSeries model.diff ( Maybe.withDefault "" newCreation.name )
                  , meta = Dict.fromList [( "tzaware", M.MBool tzawarness )]
              }
            , command
            )

        Back ->
            U.nocmd { model | mode = Creation Form }

        SwitchForceDraw ->
            applyFocus
                ( applyDiff ( buildCoord ( flipForce model )))
                ( Just (0 , 0 ))

        SwitchBatch ->
            ( { model | newBatch = not model.newBatch }
            , if not model.newBatch
              then getOffsets model
              else Cmd.none
            )

        StatVisible visible ->
            U.nocmd { model | statVisibility = visible }

        StatInfos sMsg ->
            case sMsg of
                AllowInferFreq ->
                    U.nocmd { model
                                | allowInferFreq = True
                                , statistics = getStatistics
                                               model.statistics
                                               True
                                               ( onlyActiveValues model.series )
                            }

        ShowDiff ->
            U.nocmd { model | showDiff = not model.showDiff}

        Expand expand ->
            let newModel = { model | expand = expand } in
            ( buildCoord ( cleanDiff  newModel )
            , if ( expand
                       && loadedComponents model.terminalComponents == 0
                 )
              then getDataComponents newModel True
              else Cmd.none
            )

        Visible eCol ->
            case model.nameVisbility of
                All -> U.nocmd model
                _ -> U.nocmd { model | nameVisbility = Single eCol }

        AllVisible all ->
            U.nocmd { model | nameVisbility = if all then All else Nope }

        InputChanged row col rawvalue ->
            let
                ( raw, edition ) = parseStuff rawvalue model.string
            in
            U.nocmd ( applyDiff
                          <| updateCoordData
                          model
                          ( row, col )
                          raw
                          edition
                    )

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
            case decodeSupervised rawdata of
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
                                                (Dict.keys patched)
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
            ( { model | horizon = ( setStatusPlot model.horizon Loading ) }
            , Cmd.batch
                [ patchEditedData model
                , deselect False
                ]
            )

        SaveAnyway ->
            let modelToSendNotToDraw =
                    applyDiff
                    <| buildCoord
                        { model | forceDraw = True }
            in
            ( { model | horizon = ( setStatusPlot model.horizon Loading ) }
            , Cmd.batch
                [ patchEditedData modelToSendNotToDraw
                , deselect False
                ]
            )

        CancelEdition ->
            applyFocus
                ( cleanDiff
                    ( buildCoord
                        ( cleanBatch { model | showDiff = False } )
                    )
                )
                model.focus

        Correction param ->
            case param of
                Slope value ->
                    U.nocmd ( applyDiff { model | slope = Just value } )
                Intercept value ->
                    U.nocmd ( applyDiff { model | intercept = Just value } )

        Saved (Ok _) ->
            case model.mode of
                Existing _ ->
                    ( cleanDiff model
                    , Cmd.batch ( getRelevantData model )
                    )
                Creation _ -> ( model
                              , Browser.Navigation.load
                                    <| UB.crossOrigin
                                            model.baseurl
                                            [ "tseditor" ]
                                            [ UB.string "name" model.name ]
                              )
                BasketMode ->
                    ( cleanDiff model
                    , Cmd.batch
                          ( getRelevantData model)
                    )

        Saved (Err _) ->
            U.nocmd { model | horizon = ( setStatusPlot model.horizon Failure ) }

        GotMetadata (Ok result) ->
            case JD.decodeString M.decodemeta result of
                Ok allmeta ->
                   let seriestype = if Dict.member "formula" allmeta
                                                            then I.Formula
                                                            else  I.Primary
                       horizon = model.horizon
                       newmodel = { model | meta = allmeta
                                          , tzaware = isTzaware allmeta
                                          , string = isStr allmeta
                                          , statVisibility = not (isStr allmeta)
                                          , statistics = updateFirstLast
                                                            model.statistics
                                                            allmeta
                                          , exist = True
                                          , seriestype = seriestype
                                          , mode = case seriestype of
                                              I.Primary -> Existing I.Primary
                                              I.Formula -> Existing I.Formula
                                          , horizon = { horizon | hasCache =
                                                            case seriestype of
                                                              I.Primary -> False
                                                              I.Formula -> True
                                                      }
                                  }
                   in
                   ( newmodel
                   , Cmd.batch [ model.initialCommands
                               , getsource model.baseurl model.name
                               ]
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


        Paste payload ->
            let parsed = parsePasted payload.text model.string
                coordPatch = cartesianDataRec parsed [] 0 0 0 Dict.empty
                corner = getPos payload.index
                merged = pasteRectangle model.coordData coordPatch corner model.string
            in
            U.nocmd <| applyDiff { model | rawPasted = payload.text
                                         , coordData = merged
                                  }

        UnFocus -> U.nocmd { model | focus = Nothing
                                   , selection = Nothing
                            }

        ClickCell iRow iCol ->
            case model.holding.shift of
                False -> let ( newmodel, cmd  ) = applyFocus model ( Just ( iRow, iCol ) )
                             clear =  clearSelection newmodel
                         in
                            case model.currentInput of
                                Nothing -> ({ clear | selection = Just ( pointToBox( iRow, iCol ))}
                                           , cmd )
                                Just current ->
                                    if current == ( iRow, iCol )
                                    then
                                        ( newmodel , cmd )
                                    else
                                        ( clear , cmd )
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
                Just stuff ->
                    case stuff of
                        DateRow _ -> U.nocmd model
                        Header _ -> U.nocmd model
                        Cell entry ->
                            if entry.editable
                                then U.nocmd { model | currentInput = Just ( iRow, iCol ) }
                                else U.nocmd { model | currentInput = Nothing }


        MousePosition position ->
            let hold = model.holding.mouse
                modelPos = { model | mousePosition = Just position}
                newmodel = if hold
                                then { modelPos |
                                        selection = Just <|
                                            extendSelection
                                                model.firstShift
                                                position
                                        , currentInput = Nothing
                                     }
                                else modelPos
            in
                if hold
                then
                    applyFocus ( setupFirstSelected newmodel ) ( Just position )
                else
                    U.nocmd ( setupFirstSelected newmodel )

        DeselectAll keepFocus ->
             let
                 firstShift = model.firstShift
                 newmodel = ( clearSelection model )
                 shiftModel = { newmodel | focus = firstShift
                                         , firstShift = Nothing
                              }
              in
                if keepFocus
                    then applyFocus shiftModel shiftModel.focus
                    else applyFocus shiftModel Nothing


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

        Focus ( Ok _ ) -> U.nocmd model
        Focus ( Err err ) -> case err of
                                NotFound stuff ->
                                    U.nocmd model --<| addError model "focus" stuff


        ActionControl ( key ) ->
            let holding = model.holding
                notEditing = model.currentInput == Nothing
                cond = conditionnalAction model notEditing
            in
            case key of
                Escape Down -> ( model , deselect True )
                Escape Up -> U.nocmd { model | nameVisbility = Nope }
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
                        if model.holding.control
                        then
                            U.nocmd ( applyDiff( fillSelection model ))
                        else
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
                                                            <| Dict.get
                                                                ( i, j )
                                                                ( filterEntry model.coordData )
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
                                    newTerminalComponents = List.map
                                                        (\ c -> { c | data = resetZoom c.data})
                                                        model.terminalComponents
                                    newDirectComponents = List.map
                                                        (\ c -> { c | data = resetZoom c.data})
                                                        model.directComponents
                                in
                                { model | horizon = { horizonmodel | zoomBounds = Nothing}
                                                    , series = newSeries
                                                    , directComponents = newDirectComponents
                                                    , terminalComponents = newTerminalComponents
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

                                       newDirectComponents = List.map
                                                        (\ c -> { c | data = newZoom
                                                                                minDate
                                                                                maxDate
                                                                                model.panActive
                                                                                c.data
                                                                }
                                                        )
                                                        model.directComponents
                                       newTerminalComponents = List.map
                                                        (\ c -> { c | data = newZoom
                                                                                minDate
                                                                                maxDate
                                                                                model.panActive
                                                                                c.data
                                                                }
                                                        )
                                                        model.terminalComponents
                                  in
                                    { model | series = newSeries
                                            , directComponents = newDirectComponents
                                            , terminalComponents = newTerminalComponents
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


multiStatus: List Component -> PlotStatus
multiStatus components =
    let status = List.map
                    .status
                    components
    in
    if List.any (\ elt -> elt == CompError ) status then Failure
    else
    if List.all (\ elt -> elt == CompLoaded ) status then Success
    else
    Loading


applyFocus: Model -> Maybe ( Int, Int ) -> ( Model, Cmd Msg )
applyFocus model maybeIndex =
    let
        newModel = { model | focus = maybeIndex
                           , nameVisbility = case maybeIndex of
                                            Nothing -> Nope
                                            Just ( _, fCol )
                                                -> Single fCol
                   }
    in
    case maybeIndex of
        Nothing ->
            case model.focus of
                Nothing -> ( newModel, Cmd.none )
                Just cursor ->
                    ( newModel
                    , T.attempt Focus ( Browser.Dom.blur <| idEntry cursor )
                    )
        Just index ->
            ( newModel
            , T.attempt Focus ( Browser.Dom.focus <| idEntry index )
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
    "e/" ++ ( String.fromInt i ) ++ "/"++ ( String.fromInt j )


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


onlyValues: SeriesToEdit -> Dict String ( Maybe Float )
onlyValues series =
    Dict.map
        ( \ k v -> case v.value of
                    Nothing -> Nothing
                    Just s -> case s of
                        Float f -> Just f
                        String _ -> Nothing
        )
        series

onlyStrings: SeriesToEdit -> Dict String ( Maybe String )
onlyStrings series =
    Dict.map
        ( \ k v -> case v.value of
                    Nothing -> Nothing
                    Just s -> case s of
                        Float _ -> Nothing
                        String st -> Just st
        )
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
                Nothing -> mapToFloat ts.initialTs
                Just zoom -> mapToFloat zoom
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> onlyValues ts.initialTs
                Just zoom -> onlyValues zoom

onlyActiveStrings : Series -> Dict String ( Maybe String )
onlyActiveStrings series =
    case series of
        Naked ts ->
            case ts.zoomTs of
                Nothing -> mapToString ts.initialTs
                Just zoom -> mapToString zoom
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> onlyStrings ts.initialTs
                Just zoom -> onlyStrings zoom

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


previouslyEdited: Series -> Dict String ( Maybe Float )
previouslyEdited series =
    case series of
        Naked _ -> Dict.empty
        ToEdit ts ->
            case ts.zoomTs of
                Nothing -> extractPrevious ts.initialTs
                Just zoom -> extractPrevious zoom


extractPrevious: SeriesToEdit -> Dict String ( Maybe Float )
extractPrevious ts =
    Dict.map
        (\ _ e -> case e.value of
                    Nothing -> Nothing
                    Just v ->
                        case v of
                            Float f -> Just f
                            String _ -> Nothing
        )
        <| Dict.filter
                (\ _ e -> e.override)
                ts


cleanBatch: Model -> Model
cleanBatch model =
    case model.series of
        Naked _ -> model
        ToEdit toEdit ->
            { model | series = ToEdit
                  { initialTs = Dict.filter
                                    (\ _ e -> not e.fromBatch)
                                    toEdit.initialTs
                  , zoomTs = Nothing
                  }
            }


cleanDiff: Model -> Model
cleanDiff model =
    setupFill
      { model | diff = Dict.empty
              , coordData = Dict.map
                                (\ _ s -> case s of
                                    Cell e -> Cell { e | edition = NoEdition
                                                       , raw = Nothing }
                                    DateRow d -> DateRow d
                                    Header h -> Header h
                                )
                                model.coordData
              , slope = Nothing
              , intercept = Nothing
      }

insertComponentData: List Component -> String -> CompStatus -> Maybe Series -> List Component
insertComponentData components name status data =
    List.map
        (\ comp -> if comp.name == name
                    then case data of
                        Nothing -> { comp | status = status }
                        Just series -> { comp | status = status
                                              , data = series
                                       }
                    else comp
        )
        components

cleanComponents: List Component -> List Component
cleanComponents components =
    List.map
        (\ comp -> { comp | status = CompEmpty
                          , data = emptySeries
                   }
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


getCurrentValue: Entry -> Maybe ( ScalarType Float String )
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
    let delete = (\ s ->
                    case s of
                        DateRow d -> DateRow d
                        Header d -> Header d
                        Cell e ->  if e.editable
                                        then Cell { e | edition = Deletion
                                                       , raw = Nothing
                                                  }
                                        else Cell e
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
                            ( \ s ->
                                case s of
                                    DateRow d -> DateRow d
                                    Header h -> Header h
                                    Cell e ->
                                        if e.editable
                                             then
                                                Cell { e | edition = Deletion
                                                         , raw = Nothing
                                                         }
                                             else Cell e
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
                                ( findLastValidByCol ( filterEntry coordData ))
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


fillNas: Dict ( Int, Int ) Stuff -> ( ScalarType Float String ) -> ( Int, Int ) ->  Dict ( Int, Int ) Stuff
fillNas coordData lastValue positionLastValue =
    let nbNas = getNbNas ( filterEntry coordData ) positionLastValue
    in
        Dict.map
            ( applyValue positionLastValue lastValue nbNas )
            coordData


fillSelection: Model -> Model
fillSelection model =
    case model.currentInput of
        Nothing -> model
        Just current ->
            case model.selection of
                Nothing -> model
                Just select ->
                    let val = getValueFromIndex model.coordData current
                    in fillSelected model select val


fillSelected: Model -> Box -> ScalarType Float String -> Model
fillSelected model select value =
    let
        edited = Dict.map
                    (\ pos e -> if keyInSelection select pos
                                then
                                    updateCell
                                        e
                                        value
                                else
                                    e
                    )
                    model.coordData
    in
        { model | coordData = edited }



applyValue: ( Int, Int ) -> ( ScalarType Float String )  -> Int -> ( Int, Int ) -> Stuff -> Stuff
applyValue ( rowLastValue, colLastValue) lastValue  nbNas (iRow, iCol) stuff =
    case stuff of
        DateRow d -> DateRow d
        Header h -> Header h
        Cell entry ->
            if iRow > rowLastValue && iRow <= rowLastValue + nbNas && iCol == colLastValue
                then Cell { entry | edition = Edition lastValue
                                  , raw = Just ( toRaw lastValue )}
                else Cell entry


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


fillAllNas : Dict ( Int, Int ) Stuff -> List ( Int, Int ) -> Dict ( Int, Int ) Stuff
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


cellsToString: Dict ( Int, Int ) Stuff -> Maybe Box -> String
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


rowToString: Dict ( Int, Int ) Stuff -> ( Int, Int )-> Int -> String
rowToString coordData ( minCol, maxCol ) iRow =
    String.join
        tab
        <| List.map
            ( extractValue coordData iRow )
            ( List.range minCol maxCol )


extractValue: Dict ( Int, Int ) Stuff -> Int -> Int -> String
extractValue coordData iRow iCol =
    case Dict.get ( iRow, iCol ) coordData of
        Nothing -> ""
        Just ( Cell entry ) -> showValue entry ""
        Just ( DateRow date ) -> date
        Just ( Header ( name, _ )) -> name


getValueFromIndex: Dict ( Int, Int ) Stuff -> ( Int, Int ) -> ScalarType Float String
getValueFromIndex coordData position =
    let
        stuff = Dict.get position coordData
    in
        case stuff of
            Nothing -> Float 0
            Just ( DateRow _ )  -> Float 0
            Just ( Header _ )  -> Float 0
            Just ( Cell entry ) -> Maybe.withDefault
                                    ( Float 0 )
                                    ( getCurrentValue entry )


bounded: ( ( Int, Int ), ( Int, Int )) -> ( Int, Int ) -> ( Int, Int )
bounded ( ( minRow, maxRow ), ( minCol, maxCol )) ( pRow, pCol) =
    (( simpleBound minRow maxRow pRow )
    ,( simpleBound ( minCol ) maxCol pCol ) )


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
        BasketMode ->  [ getDataComponents model False ]


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
                    Edition ( Float val )
                Nothing ->
                    Error value

parseString : String -> Edited
parseString value =
    if value == ""
        then Deletion
        else
            Edition ( String value )


parseStuff: String -> Bool -> (Maybe String, Edited)
parseStuff raw isString =
    if not isString
    then
        let rawstring = String.replace " " "" raw
            edition = parseInput rawstring
            cleanedRaw = if rawstring == ""
                            then Nothing
                            else Just rawstring
        in ( cleanedRaw, edition )
    else
        let edition = parseString raw
            cleanedRaw = if raw == ""
                then Nothing
                else Just raw
        in ( cleanedRaw, edition )


cartesianDataRec:  List ( List a ) ->  List a -> Int -> Int -> Int -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
cartesianDataRec mergedData currentRow minCol i j myDict =
        if j == minCol
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
                                minCol
                                i
                                ( j + 1 )
                                (Dict.insert (i, j) cell myDict)

        else
          case currentRow of
                [] -> cartesianDataRec
                        mergedData
                        []
                        minCol
                        ( i + 1 )
                        minCol
                        myDict
                cell :: cells ->
                    cartesianDataRec
                        mergedData
                        cells
                        minCol
                        i
                        ( j + 1 )
                        (Dict.insert (i, j) cell myDict)


cartesianData: List ( List a )-> Dict ( Int, Int ) a
cartesianData mergedData =
    cartesianDataRec mergedData [] -1 -1 -1 Dict.empty


mergeData: List Component -> List ( List Stuff )
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
                    ( \ c -> ( c.name, c.cType ) )
                    components
    in
        [[ Header ( "Dates", Primary ) ] ++ List.map ( \ s -> Header s ) columns ]
         ++ ( List.map
                ( builRowBasic components )
                dates
            )


builRowBasic: List Component -> String -> List Stuff
builRowBasic components date =
    [ DateRow date ]
         ++ ( List.map
                ( getStuff date )
                <| List.map
                    (\ c -> ( c.name , c.data )
                    )
                    components
            )


applyDiff: Model -> Model
applyDiff model =
    setupFill { model | diff = currentDiff model ( filterEntry model.coordData )}


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


patchCurrent : SeriesToEdit -> SeriesNaked -> String -> SeriesToEdit
patchCurrent base patch name  =
    let baseEntry = { raw = Nothing
                    , value = Nothing
                    , edition = Deletion
                    , editable = True
                    , override = False
                    , indexRow = ""
                    , indexCol = name
                    , fromBatch = True
                    }
    in
    Dict.merge
        ( \_ _ r -> r )
        ( \ d b p r -> case p of
                        Nothing -> Dict.insert d b r
                        Just v -> Dict.insert
                                    d
                                    { b
                                    | raw = Just ( toRaw v )
                                    , edition = Edition v
                                    }
                                    r
        )
        ( \ d p r -> case p of
                        Nothing -> Dict.insert
                                    d
                                    { baseEntry
                                    | indexCol = d
                                    }
                                    r
                        Just v -> Dict.insert
                                    d
                                    { baseEntry
                                    | indexCol = d
                                    , raw = Just ( toRaw v )
                                    , edition = Edition v
                                    }
                                    r
        )
        base
        patch
        base


pasteRectangle: Dict ( Int, Int ) Stuff -> Dict ( Int, Int ) String -> ( Int, Int ) -> Bool -> Dict ( Int, Int ) Stuff
pasteRectangle base patch ( cornerRow, cornerCol ) isString =
    let translatedPatch = Dict.fromList
                            <| List.map
                                (\ (( i, j ), v ) ->
                                    (( i + cornerRow, j + cornerCol ), v ))
                                ( Dict.toList patch )
    in
        Dict.merge
            ( \_ _ dict -> dict )
            ( \ position stuffBase sPatch dict ->
                case stuffBase of
                    DateRow _ -> dict
                    Header _ -> dict
                    Cell e ->
                        if e.editable
                            then
                                Dict.insert
                                    position
                                    ( patchEntry stuffBase sPatch isString )
                                    dict
                            else dict
            )
            ( \_ _ dict -> dict )
            base
            translatedPatch
            base


patchEntry: Stuff -> String -> Bool ->Stuff
patchEntry stuff s isString =
    case stuff of
        DateRow date -> DateRow date
        Header h -> Header h
        Cell entry -> Cell { entry | raw = Just s
                                    , edition = if not isString
                                                then parseInput s
                                                else parseString s
                            }

relevantComponents: Model -> List Component
relevantComponents model =
    if model.expand
        then model.terminalComponents
        else model.directComponents

patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let allSeries = likeComp model ++ relevantComponents model
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
            case v of
                String _ -> Edition v
                Float f ->
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
                                    Just inter -> Edition (Float ( f + inter ))
                            Just slope ->
                                case b of
                                    Nothing -> Edition (Float ( f * slope ))
                                    Just inter ->
                                        Edition (Float ( f * slope + inter ))


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
                            Edition val ->
                                case val of
                                    Float f ->
                                        [(e.indexRow, Just f)]
                                    String s ->
                                        [(e.indexRow, Nothing )]
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
        <| case model.mode of
                Creation Form -> [ ]
                Existing I.Primary ->
                    [ roundForm model
                     , buttonShowDiff model
                     , buttonFillAll model
                     ]
                BasketMode ->
                    [ roundForm model
                     , buttonShowDiff model
                     , buttonFillAll model
                     , buttonViewNames model

                     ]
                _ -> [ roundForm model
                     , buttonShowDiff model
                     , buttonFillAll model
                     , buttonExpandFormula model
                     , buttonViewNames model
                     ]


permaLink: Model -> H.Html Msg
permaLink model =
    H.a
        [ HA.href ( UB.crossOrigin
                        model.baseurl
                        ["tseditor"]
                        <| case model.mode of
                            BasketMode -> basketLink model
                            _ ->
                                ( queryNav model model.name )
                  )
        , HA.target "_blank"
        , HA.class "permalink"
        ]
        [ H.text "permalink"]


roundForm: Model -> H.Html Msg
roundForm model =
    H.div
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


printComptStatus compStatus =
    case compStatus of
           CompLoaded -> "Complete"
           CompError -> "Error"
           CompEmpty -> "Empty"


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


buttonExpandFormula: Model -> H.Html Msg
buttonExpandFormula model =
       H.div
        [ HA.class "custom-control custom-switch"
        , HA.class "button-expand"
        , HA.title ( hoverExpand model )
        ]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "expandFormula"
            , HA.checked ( model.expand )
            , HE.onCheck Expand
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "expandFormula"
            ]
            [ H.text ( "Expand Formula" ++ ( infoExpand model ))]
        ]


infoExpand: Model -> String
infoExpand model =
    let nbD = if model.expand
                then loadedComponents model.terminalComponents
                else List.length model.directComponents
        nbT = List.length model.terminalComponents
        left = if nbD==0
                    then "..."
                    else String.fromInt nbD
        right =  if nbT== 0
                    then "..."
                    else String.fromInt nbT
    in
        "(" ++ left ++ "/" ++ right ++ ")"


loadedComponents: List Component -> Int
loadedComponents components =
    List.count
        (\ c -> case c.status of
                    CompLoaded -> True
                    _ -> False
        )
        components


hoverExpand: Model -> String
hoverExpand model =
    let nbD = List.length model.directComponents
        nbT = List.length model.terminalComponents
    in
         String.fromInt nbD
         ++ " direct components, "
         ++ String.fromInt nbT
         ++ " terminal components"


buttonViewNames: Model -> H.Html Msg
buttonViewNames model =
     H.div
        [ HA.class "custom-control custom-switch"
        , HA.class "button-col-visible"
        ]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "expandAll"
            , HA.checked ( model.nameVisbility == All )
            , HE.onCheck AllVisible
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "expandAll"
            ]
            [ H.text "View Names" ]
        ]



viewRelevantTable: Model -> H.Html Msg
viewRelevantTable model =
    case model.mode of
        Existing I.Primary -> viewValueTable model
        Existing I.Formula -> viewValueTable model
        BasketMode ->  viewValueTable model
        Creation Form -> H.table
                            [ HA.class "creation-form"
                            ,  HE.onClick UnFocus
                            ]
                            <| [ nameForm model ] ++ ( creationForm
                                                        model
                                                        True
                                                        FromScratch
                                                      )

        Creation Edit -> H.div
                            [ ]
                            [ H.table
                                [ HA.class "creation-form"
                                , HE.onClick UnFocus
                                ]
                                [ nameForm model ]
                            , viewValueTable model ]


checkMandatory: Maybe String -> String
checkMandatory s =
    case s of
        Nothing -> "mandatory"
        Just _ ->  ""


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
        <| List.concat [
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
                    , HE.onInput ( \s -> Create ( Name s ) )
                    , HA.value ( Maybe.withDefault "" model.creation.name )
                    ]
                    []
                ]
            ]
            , ( if model.mode == Creation Edit
                then [ backButton model ]
                else []
               )
        ]


backButton: Model -> H.Html Msg
backButton model =
     H.button
        [ HA.class "yellowbutton top-button custom-button"
        , HA.id "back-button"
        , HE.onClick Back
        ]
        [ H.text "Back to Form" ]


creationForm: Model -> Bool -> PreviewType -> List ( H.Html Msg )
creationForm model showTz previewType =
        [ H.tr
            [ ]
            [ H.td
                []
                [ H.label
                    [HA.for "creation-from"]
                    [H.text "From "]
                ]
            , H.td
                []
                [ H.input
                    [ HA.type_ "datetime-local"
                    , HA.id "creation-from"
                    , HA.class "input-date"
                    , HA.class  ( checkMandatory model.creation.from )
                    , HA.name "from"
                    , HE.onInput ( \s -> Create ( From s ) )
                    , HA.value ( Maybe.withDefault "" model.creation.from )
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
                    [ HA.type_  "datetime-local"
                    , HA.id "creation-to"
                    , HA.class "input-date"
                    , HA.class  ( checkMandatory model.creation.to )
                    , HA.name "to"
                    , HE.onInput ( \s -> Create ( To s ) )
                    , HA.value ( Maybe.withDefault "" model.creation.to )
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
                    showTz
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
                                    Just f -> f
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
                    [ HE.onClick ( Create ( Preview previewType ) )
                    , HA.disabled ( not model.creation.mandatoryValid )
                    , HA.class "bluebutton custom-button"
                    ]
                    [ H.text "Preview" ]
                ]
            ]
        ]

tzoneDropdown : List String-> TzSelector -> String -> Bool -> H.Html Msg
tzoneDropdown choices selected fromHorizon showTz =
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
        <| if showTz
            then ( List.map
                    ( renderTimeZone selected fromHorizon )
                    ( naiveTag::choices )
                  )
            else ( List.map
                    ( renderTimeZone selected fromHorizon )
                    [ naiveTag ]
                 )


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
                [ msgTooManyPointsWithButton nb
                , saveAnyway model ]
        Drawable ->
            H.div
                []
                (if isSingle model
                then
                   [ H.div
                        [ HA.class "wrapper-primary" ]
                        [ H.div
                            []
                            [ buildTable model
                            , addPatch model
                            ]
                        , strap
                        , forCurrentDiff model
                        ]
                    ]
                else
                    [ forCurrentDiff model
                    , buildTable
                        model
                    ]
                )

strap =
    H.div
        [ HA.class "separator-primary" ]
        []


addPatch: Model -> H.Html Msg
addPatch model =
    if not model.newBatch
    then H.button
            [ HA.class "bluebutton"
            , HE.onClick SwitchBatch ]
            [ H.text "Add Data Batch "]
    else
        H.div
            [ HE.onClick UnFocus ]
            [ H.button
                [ HA.class "yellowbutton"
                , HE.onClick SwitchBatch ]
                [ H.text "Hide"]
            , H.table
                [ ]
                ( creationForm model model.tzaware Patch )
            ]



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


getBounds:  Dict ( Int, Int ) a -> ( ( Int, Int ),  ( Int, Int ))
getBounds cartDict =
    let coords = Dict.keys cartDict
        rows = List.map Tuple.first coords
        cols = List.map Tuple.second coords
        minRow = Maybe.withDefault -1 <| List.minimum rows
        maxRow = Maybe.withDefault -1 <| List.maximum rows
        minCol = Maybe.withDefault -1 <| List.minimum cols
        maxCol = Maybe.withDefault -1 <| List.maximum cols
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
            let allSeries = likeComp model ++ relevantComponents model
                dataAsRows = mergeData allSeries
            in
            setupFill
                { model | coordData = ( cartesianData dataAsRows)
                }


updateCoordData: Model -> (Int, Int) -> Maybe String -> Edited -> Model
updateCoordData model position raw edition =
    let previous = Dict.get position model.coordData
    in
        case previous of
            Nothing -> model
            Just  ( DateRow _ ) -> model
            Just  ( Header _ ) -> model
            Just ( Cell entry) ->
               if not entry.editable
                        then model
                        else
                            let
                                newCoord = Dict.insert
                                            position
                                            ( Cell { entry | raw = raw
                                                       , edition = edition
                                                    }
                                            )
                                            model.coordData
                            in
                                { model | coordData = newCoord }

updateCell : Stuff -> ScalarType Float String -> Stuff
updateCell stuff value =
    case stuff of
        DateRow _  -> stuff
        Header _  -> stuff
        Cell entry ->
           if not entry.editable
                then stuff
                else
                    Cell { entry | raw = Just (toRaw value)
                                 , edition = Edition value
                         }


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
                then "Hide Diff"
                else "Show Diff"
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
        H.td [] [ H.text <| getValue entry "-"]


buildTable: Model ->  H.Html Msg
buildTable model =
    let cartDict = model.coordData
        ( ( minRow, maxRow ), ( minCol, maxCol )) = getBounds cartDict
    in
        H.table
        [ HA.class "edit-table"
        , HA.class "unselectable"]
        ( List.map
            ( buildCartRow model cartDict minCol maxCol )
            ( List.range minRow maxRow ) )


buildCartRow: Model -> Dict ( Int, Int ) Stuff -> Int -> Int -> Int -> H.Html Msg
buildCartRow model cartDict minCol maxCol iRow =
    H.tr
        [ HA.class "row-edit" ]
        <| List.map
                ( buildAny model cartDict iRow )
                ( List.range minCol maxCol )


buildAny: Model -> Dict ( Int, Int ) Stuff -> Int -> Int -> H.Html Msg
buildAny model cartDict iRow iCol =
    case Dict.get ( iRow, iCol ) cartDict of
            Nothing -> buildCell model emptyEntry iRow iCol
            Just ( Cell entry ) -> buildCell model entry iRow iCol
            Just ( DateRow date ) -> buildDateCell model date iRow
            Just ( Header header ) -> buildHeader model header iCol


buildHeader: Model -> ( String, CType ) -> Int -> H.Html Msg
buildHeader model ( name, cType ) iCol =
     let focused = case model.focus of
                    Nothing -> False
                    Just f -> f == ( -1, iCol )
         selected = case model.selection of
                    Nothing -> False
                    Just box -> keyInSelection box ( -1, iCol )
    in
    H.th
        [ HA.tabindex 0 --allow to be focusable
        ,  HA.id ( idEntry ( -1, iCol ) )
        , HA.class "column-header"
        , if focused
                then HA.class "focused"
                else HA.class ""
             , if selected
                then HA.class "selected"
                else HA.class ""
         , HE.onClick ( ClickCell -1 iCol )
         , HE.onMouseOver ( Visible iCol )
         , HE.onMouseDown ( Drag ( On ( -1, iCol ) ))
         , HE.onMouseEnter ( MousePosition ( -1, iCol ) )
         ]
        ( insideHeader model name cType iCol )


insideHeader: Model -> String -> CType -> Int -> List ( H.Html Msg )
insideHeader model name cType iCol =
    let indexPositive = iCol + 1
    in
    case indexPositive of
        0 -> [ H.p
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
        1 ->
            case model.mode of
                BasketMode ->
                    buildLink model iCol name cType
                _ ->
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
        _ -> buildLink model iCol name cType


buildDateCell: Model -> String -> Int -> H.Html Msg
buildDateCell model date iRow =
     let focused = case model.focus of
                    Nothing -> False
                    Just f -> f ==  ( iRow, -1 )
         selected = case model.selection of
                    Nothing -> False
                    Just box -> keyInSelection box ( iRow, -1 )
    in
        H.th
            [ HA.class "show-table-dates"
             , if focused
                then HA.class "focused"
                else HA.class ""
             , if selected
                then HA.class "selected"
                else HA.class ""
             , HE.onClick ( ClickCell iRow -1 )
             , HE.onMouseDown ( Drag ( On ( iRow, -1 ) ))
             , HE.onMouseEnter ( MousePosition ( iRow, -1 ))]
            [ H.node
                "batch-copy"
                [ HA.attribute "index" ( idEntry (iRow, -1) )
                , HE.on "pastewithdata" (JD.map Paste pasteWithDataDecoder)
                , HA.tabindex 0 --allow to be focusable
                , HA.id ( idEntry (iRow, -1) )
                ]
                [ H.text date ]
            ]


buildCell: Model -> Entry -> Int -> Int -> H.Html Msg
buildCell model entry iRow iCol  =
    let
        statusClass = cellStyle entry
        focused = case model.focus of
                    Nothing -> False
                    Just f -> f ==  ( iRow, iCol )
        selected = case model.selection of
                    Nothing -> False
                    Just box -> keyInSelection box ( iRow, iCol )
        fillUnder =  List.member ( iRow, iCol )  model.lastValids
        debug = model.horizon.debug
        active = case model.currentInput of
                    Nothing -> False
                    Just position -> (iRow, iCol) == position
        value = showValue entry ( if active then "" else "-" )
        valueCropped = printValue model.roundValues value
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
        , HE.onMouseEnter ( MousePosition ( iRow, iCol ))
        ]
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
                    , HA.title statusClass
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
                    , HA.title statusClass
                    , HA.class "unselectable"
                    , HA.value valueCropped
                    , HA.readonly True
                    ]
                    [ ]
            ]


getStuff: String ->  ( String , Series ) -> Stuff
getStuff date ( name, series ) =
    Cell ( getEntry date ( name, series ))


getEntry: String ->  ( String , Series ) -> Entry
getEntry date ( name, series ) =
    let defaultEntry = { raw = Nothing
                       , value = Nothing
                       , edition = NoEdition
                       , editable = False
                       , override = False
                       , indexRow = date
                       , indexCol = name
                       , fromBatch = False
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
                                                                    ( Just toRaw )
                                           }
                        Nothing -> defaultEntry
                Nothing ->
                    case Dict.get date ts.initialTs of
                        Just value ->  { defaultEntry | value = value
                                                      , raw = Maybe.andMap
                                                                value
                                                                ( Just toRaw )
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
                                                        (Just toRaw)
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
                                                        (Just toRaw)
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
            ( relevantComponents model )
        )


datesComponent: Model -> Component -> Set String
datesComponent model comp =
    let allDates = onlyActiveKeys comp.data
    in
        Set.fromList allDates


basketLink: Model -> List UB.QueryParameter
basketLink model =
    let bounds = getFromToDates model.horizon
        base = UB.string "basket" model.basket
    in
    case bounds of
        Nothing -> [ base ]
        Just ( min, max )-> [ base
                            , UB.string "startdate" min
                            , UB.string "enddate" max
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


buildLink: Model -> Int -> String -> CType -> List ( H.Html Msg )
buildLink model iCol name cType =
    let class = case model.nameVisbility of
            All -> "all-expanded"
            Nope -> "hidden"
            Single eCol ->
                case iCol == eCol of
                            True -> "expanded"
                            False -> "hidden"
    in
    ( case cType of
        Auto ->
            [ H.p [] [ H.text name ]]
        _ ->
            [ H.a
                [ HA.class class
                , HA.href ( UB.crossOrigin model.baseurl
                                [ "tseditor" ]
                                ( queryNav model name ))
                ]
                [ H.p
                    [ ]
                    [ H.text name ]
                ]
            ]
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
        Edition edit ->
            case edit of
                String _ -> Nothing
                Float f -> Just f


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


saveAnyway : Model -> H.Html Msg
saveAnyway model =
    case model.mode of
        Creation _->
            H.button
              [ HA.class "greenbutton custom-button save-button"
              , HA.attribute "type" "button"
              , HE.onClick SaveAnyway
              , HA.disabled ( disableSave model )
              ]
              [ H.text ( printStatus model.horizon.plotStatus ) ]
        _ -> H.div [ HA.class "placeholder-save-anyway" ] []


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


getValue : Entry -> String -> String
getValue entry blankStr =
     case entry.edition of
                Edition v -> toRaw v
                Error s -> s
                Deletion -> blankStr
                NoEdition ->
                    case entry.value of
                        Nothing -> ""
                        Just v ->
                            case v of
                                String s -> s
                                Float f ->
                                    String.fromFloat f


showValue: Entry -> String -> String
showValue entry blankStr =
    case entry.raw of
        Nothing-> getValue entry blankStr
        Just stuff -> stuff


cellStyle: Entry -> String
cellStyle entry =
        case entry.edition of
            Edition _ -> "editing"
            Deletion -> "nan"
            Error _ -> "invalid"
            NoEdition ->
                if entry.override
                 then "overridden"
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
            , HA.attribute "value" ( displayRaw ( Maybe.map toRaw entry.value ))
            , HA.attribute "edition" ( displayEdition entry.edition )
            , HA.attribute "overridden"  <| if entry.override then "True" else "False"
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
        Edition value -> "Edition-" ++ toRaw value
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
                , H.text ( "Name : " ++ model.name )
                , H.br [] []
                , H.text ( "Basket : " ++ model.basket )
                , H.br [] []
                , H.text ( "Series Basket : "
                          ++ String.join
                                " / "
                                (List.map .name model.directComponents )
                          )
                , H.br [] []
                , H.div [] <| List.map
                                (\  c ->  H.div []
                                            [H.br [] []
                                            , H.text
                                                (  c.name
                                                    ++ " : "
                                                    ++
                                                    printComptStatus c.status
                                                )
                                            ]
                                )
                                model.directComponents
                , H.br [] []
                , H.text ( "Raw pasted : " ++ splitRaw model.rawPasted )
                , ( Markdown.toHtmlWith option [] model.rawPasted )
                , H.br [] []
                , H.text ("Last Date: " ++ ( getLastNaive <| case model.series of
                                                                Naked series -> Dict.keys series.initialTs
                                                                ToEdit series -> Dict.keys series.initialTs
                                            )
                          )
                , H.br [] []
                , H.text (", Mouse position: " ++ case model.mousePosition of
                                Nothing -> "Nothing"
                                Just pos -> displayCoord pos
                        )
                , H.br [] []
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


buttonStat : Model -> H.Html Msg
buttonStat model =
    let visible =  model.statVisibility
    in
       H.div
        [ HA.class "button-stat"
        , HA.class (if visible then "s-open" else "s-close")
        , HA.title (if visible then "Hide stats" else "Show stats")
        , HE.onClick ( StatVisible ( not visible ))
        ]
        ( if visible
            then [ H.text "►" ]
            else [ H.text "◄" ]
        )


displayStatus: Model -> H.Html Msg
displayStatus model =
    if model.horizon.plotStatus == None
      then H.text ""
      else if isEmpty model
                && (model.horizon.plotStatus == Success)
                && model.mode /= BasketMode
           then H.text """No data in this interval: select another one."""
           else H.text ""

msgDoesNotExist =
     H.div
        []
        [ H.text "Series does not exists. Check your url."]


plotNode: Model -> H.Html Msg
plotNode model =
    let dragMode =
            if model.panActive
            then "pan"
            else "zoom"
        hoverFormat = Just <| buildFormater
                                    model.roundValues
        lineMarker = ( if model.horizon.inferredFreq
                            then "lines+markers"
                            else "lines" )
        yaxis = defaultLayoutOptions.yaxis
        newYaxis =  { yaxis | hoverFormat = hoverFormat
                    }
        xaxis = defaultLayoutOptions.xaxis
        newXaxis = { xaxis
                    | range = extractDates
                                <| getFromToDates model.horizon
                   }
    in
    case model.mode
        of BasketMode -> plotBasket model dragMode lineMarker newXaxis newYaxis
           _ ->
               case model.string of
                   True -> plotString model
                   False ->
                    plotSingle model dragMode lineMarker newXaxis newYaxis



plotSingle: Model -> String -> String -> Axis -> Axis -> H.Html Msg
plotSingle model dragMode lineMarker xAxis yAxis =
    let dates = onlyActiveKeys model.series
        values = Dict.values ( onlyActiveValues model.series )
        diff = Dict.values ( currentDiff model ( filterEntry model.coordData ))
        previous = previouslyEdited  model.series
        editionTrace = case model.mode of
                        Existing I.Formula -> []
                        _ -> [ showEdition diff
                             ]
        previouslyEditedTrace = case model.mode of
            Existing I.Primary ->
                [ scatterplot
                    "manual"
                    ( Dict.keys previous )
                    ( Dict.values previous )
                    "markers"
                    defaultTraceOptions
                ]
            _ -> []

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
                       lineMarker
                        defaultTraceOptions
                      ] ++ editionTrace
                        ++ previouslyEditedTrace
                    )
                    { defaultLayoutOptions | dragMode = Just dragMode
                                           , yaxis = yAxis
                                           , xaxis = xAxis
                                           , height = Just 350
                    }
                    defaultConfigOptions
                )
            ]
            [ ]


plotString: Model -> H.Html Msg
plotString model =
    let dates = onlyActiveKeys model.series
        values = Dict.values ( onlyActiveStrings model.series )
        pseudoTs = List.map
                    (\ v -> case v of
                                Nothing -> Nothing
                                Just str ->
                                    if str == ""
                                        then Just 0
                                        else Just 1
                    )
                    values
    in
     H.div
     []
     [ H.div [ HA.id "plot" ] [ ]
     , H.node "plot-figure"
            [ HA.attribute
                "args"
                ( serializedPlotArgs
                     "plot"
                    ( [ scatterplot
                        "Series String"
                        dates
                        pseudoTs
                        "markers"
                        defaultTraceOptions
                      ]
                    )
                    { defaultLayoutOptions |
                        xaxis = { defaultDateAxis
                              | range = extractDates model.horizon.zoomBounds
                          }
                        , dragMode = Just ( if model.panActive then "pan" else "zoom" )
                        , height = Just 200
                    }
                    defaultConfigOptions
                )
            ]
            [ ]
    ]


plotBasket: Model -> String -> String -> Axis -> Axis -> H.Html Msg
plotBasket model dragMode lineMarker xAxis yAxis =
    let diff = Dict.values ( currentDiff model ( filterEntry model.coordData ))
    in
        H.node "plot-figure"
            [ HA.attribute
                "args"
                ( serializedPlotArgs
                     "plot"
                    ( List.map
                        ( traceComp lineMarker )
                        model.directComponents
                     ++ [ showEdition diff ]
                    )
                    { defaultLayoutOptions | dragMode = Just dragMode
                                           , yaxis = yAxis
                                           , xaxis = xAxis
                                           , height = Just 350
                    }
                    defaultConfigOptions
                )
            ]
            [ ]



showEdition diff =
    scatterplot
        "edition"
        ( List.map
            (\ e -> e.indexRow)
            diff
        )
        ( diffToFloat diff )
        "markers"
        defaultTraceOptions


traceComp : String -> Component -> Trace
traceComp lineMarker component =
    let dates = onlyActiveKeys component.data
        values = Dict.values  ( onlyActiveValues component.data )
    in
        scatterplot
            component.name
            dates
            values
            lineMarker
            { defaultTraceOptions | showlegend = True }


tableStat: Model -> H.Html Msg
tableStat model =
    case model.mode of
        BasketMode -> H.div [] []
        _ -> H.div
                [ HA.class "stat-table-container"]
                ( [ buttonStat model ]
                  ++ if model.statVisibility
                        then  [ viewStatTable
                                    model.statistics
                                    model.roundStat
                                    convertStat
                              ]
                        else []
                )


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
                , underThePlot model
                ]
                , tableStat model
                ]
        , debugView model
        , viewRelevantTable model
        , H.div [] ( List.map (\ err -> H.p [] [H.text err]) model.errors)
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
        BasketMode -> getBasket model.baseurl model.basket


type alias Input =
    { baseurl : String
    , name : String
    , basket: String
    , min: String
    , max: String
    , debug: String
    }


init : Input -> ( Model, Cmd Msg )
init input =
     ({ baseurl = input.baseurl
                    , errors = [ ]
                    , name = input.name
                    , basket = input.basket
                    , mode = if input.name /= ""
                                then Existing I.Primary
                                else
                                if input.basket /= ""
                                    then BasketMode
                                    else Creation Form
                    , meta = Dict.empty
                    , exist = False
                    , tzaware = True
                    , string = False
                    , source = ""
                    , seriestype = I.Primary
                    , horizon = initHorizon
                                    input.baseurl
                                    input.min
                                    input.max
                                    input.debug
                                    None
                    , creation = initCreationModel
                    , newBatch = False
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
                    , mousePosition = Nothing
                    , focus = Nothing
                    , firstShift = Nothing
                    , firstSelected = Nothing
                    , currentInput = Nothing
                    , nameVisbility = Nope
                    , holding = emptyHolding
                    , keyName = ""
                    , lastValids = []
                    , series = emptySeries
                    , statistics = emptyStat
                    , roundStat = 2
                    , statVisibility = True
                    , roundValues = Nothing
                    , statusCopy = initialStatusCopy
                    , panActive = False
                    , expand = False
                    , directComponents = []
                    , terminalComponents = []
                    , coordData = Dict.empty
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

