port module Tsinfo exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation exposing (load)
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import NavTabs exposing
    ( header
    , tabcontents
    , Tabs(..)
    , strseries
    , DeleteEvents
    , MetaEvents
    )
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , ZoomFromPlotly
    , extractDates
    , extractValues
    , getFromToDates
    , getFetchBounds
    , getIdates
    , initHorizon
    , loadFromLocalStorage
    , updateHorizon
    , updateHorizonFromData
    , extractZoomDates
    , setStatusPlot
    , updateZoom
    )
import Horizon as HorizonModule
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Info as I
import Info exposing
    ( SeriesType(..)
    , Direction(..)
    )
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import List.Selection as LS
import Maybe.Extra as Maybe
import Metadata as M
import Metadata exposing ( Metadata )
import OrderedDict as OD
import Plotter exposing
    ( defaultConfigOptions
    , defaultDateAxis
    , defaultLayoutOptions
    , defaultTraceOptions
    , defaultValueAxis
    , getdata
    , scatterplot
    , seriesdecoder
    , serializedPlotArgs
    )
import Process as P
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
import Time
import Date

port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg
port dataFromHover : ( String -> msg ) -> Sub msg
port copyToClipboard : String -> Cmd msg
port panActive : (Bool -> msg) -> Sub msg

defaultRevMaxPrimary = 70
defaultRevMaxFormula = 10


type alias Model =
    { baseurl : String
    , name : String
    , source : String
    , tzaware : Bool
    , activetab : Tabs
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    , doesnotexist: Bool
    -- metadata, ventilated by std (system) and user
    , meta : M.Metadata
    , usermeta : M.Metadata
    , seriestype : I.SeriesType
    , timeseries: TimeSeries
    , statistics: StatInfos
    , allowInferFreq: Bool
    , roundStat: Int
    -- formula
    , formula_depth : Int
    , formula_maxdepth : Int
    , formula : Dict Int String
    -- cache
    , policy : M.Metadata
    , deleting_cache : Bool
    -- log
    , log : List I.Logentry
    , logsNumber : Maybe Int
    -- plot
    , insertion_dates : Array String
    , date_index : Int
    , date_index_deb : Debouncer Msg
    , panActive: Bool
    -- user meta edition
    , metaitem : (String, String)
    , editeditems : Dict String String
    -- deletion
    , deleting : Bool
    -- renaming
    , renaming : Bool
    , newname : Maybe String
    -- clipboard
    , clipboardclass : String
    -- horizon
    , horizon : HorizonModel
    -- history mode
    , wipe: Bool
    , integrity: Int
    , historyPlots : Dict String (Dict String (Maybe Float))
    , historyMode : Bool
    , historyIdates : Array String
    , lastIdates : Array String
    , historyDateIndex : Int
    , nbRevisions: Int
    , maxNbRevisions: Maybe Int
    , previousMax: Int
    , historyDateIndexDeb : Debouncer Msg
    , dataFromHover : Maybe DataFromHover
    , debug : Bool
    }


type alias RenameEvents =
    { confirmrename : Msg
    , editnewname : String -> Msg
    , cancelrename : Msg
    , askrename : Msg
    }


type Msg
    = GotSysMeta (Result Http.Error String)
    | GotUserMeta (Result Http.Error String)
    | GotSource (Result Http.Error String)
    -- tabs
    | Tab Tabs
    -- perms
    | GetPermissions (Result Http.Error String)
    -- data
    | GotLog (Result Http.Error String)
    | GotPlotData (Result Http.Error String)
    | StatInfos ModuleStatInfos.Msg
    -- dates
    | ChangedIdate String
    | DebounceChangedIdate (Debouncer.Msg Msg)
    | IdatePickerChanged String
    -- formula
    | GotFormula (Result Http.Error String)
    | InsertionDates (Result Http.Error String)
    | GotDepth (Result Http.Error String)
    | SwitchLevel String
    -- cache
    | HasCache (Result Http.Error String)
    | DeleteCache
    | CacheCancelDeletion
    | CacheConfirmDeletion
    | CacheDeleted (Result Http.Error String)
    | GotCachePolicy (Result Http.Error String)
    -- metadata edition
    | MetaEditAsked
    | MetaEditCancel
    | MetaItemToDelete String
    | EditedValue String String
    | NewValue String
    | NewKey String
    | AddMetaItem
    | SaveMeta
    | MetaSaved (Result Http.Error String)
    -- deletion
    | AskDeletion
    | CancelDeletion
    | ConfirmDeletion
    | Deleted (Result Http.Error String)
    -- renaming
    | AskRename
    | EditNewName String
    | ConfirmRename
    | CancelRename
    | Renamed (Result Http.Error String)
    -- clipboard
    | CopyNameToClipboard
    | ResetClipboardClass
    -- horizon
    | Horizon HorizonModule.Msg
    -- history mode
    | HistoryMode Bool
    | Transitory
    | FromZoom ZoomFromPlotly
    | NewDragMode Bool
    | HistoryIdates (Result Http.Error String)
    | GotVersion Int String (Result Http.Error String)
    | DebounceChangedHistoryIdate (Debouncer.Msg Msg)
    | ChangedHistoryIdate String
    | IterIDate Bool Direction
    | ViewAllHistory
    | ChangeMaxRevs String
    | UpdateMax
    | NewDataFromHover String
    | LogsNumber String
    | SeeLogs


convertMsg : HorizonModule.Msg -> Msg
convertMsg msg =
    Horizon msg

convertStat : ModuleStatInfos.Msg -> Msg
convertStat msg =
    StatInfos msg


type TimeSeries
    = SeriesFloat (Dict String ( Maybe Float ))
    | SeriesString (Dict String String)


type alias DataFromHover =
    { name : String
    , data : List DataItem
    }


type alias DataItem =
    { date : String
    , value : Float
    }


setStatus: Model -> PlotStatus -> Model
setStatus model status =
    let horizon = model.horizon
    in
        { model | horizon = { horizon | plotStatus = status }}

stringseriesdecoder =
    --infer freq can produce null for string
    D.dict <| D.map
                ( Maybe.withDefault "" )
                (D.nullable D.string)


decodeAndType: String -> Result ( D.Error, D.Error ) TimeSeries
decodeAndType raw =
    case D.decodeString seriesdecoder raw  of
        Ok ts -> Ok ( SeriesFloat ts )
        Err errFloat ->
            case D.decodeString stringseriesdecoder raw of
                Ok ts -> Ok ( SeriesString ts )
                Err errString -> Err ( errFloat, errString )


dataItemDecoder : D.Decoder DataItem
dataItemDecoder =
    D.map2 DataItem
        (D.field "date" D.string)
        (D.field "value" D.float)


dataFromHoverDecoder : D.Decoder DataFromHover
dataFromHoverDecoder =
    D.map2 DataFromHover
        (D.field "name" D.string)
        (D.field "data" (D.list dataItemDecoder))


removeRedundants: DataFromHover -> DataFromHover
removeRedundants dataHover =
     { name = dataHover.name
     , data = removerepeated dataHover.data Nothing
     }


removerepeated: List DataItem -> Maybe Float -> List DataItem
removerepeated data previous =
    case data of
        [] -> []
        x :: xs  ->
            case previous of
                Nothing ->
                    [ x ] ++ ( removerepeated xs ( Just x.value ))
                Just prev ->
                    if prev /= x.value
                    then
                        [ x ] ++ ( removerepeated xs <| Just x.value )
                    else
                        removerepeated xs <| Just x.value


getdepth : Model -> Cmd Msg
getdepth model =
    Http.get
        { expect = Http.expectString GotDepth
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "formula_depth" ]
              [ UB.string "name" model.name ]
        }


getsource : String -> String -> Cmd Msg
getsource baseurl name =
    Http.get
        { expect = Http.expectString GotSource
        , url = UB.crossOrigin baseurl
              [ "api", "series", "source" ]
              [ UB.string "name" name ]
        }


getplot : Model -> Cmd Msg
getplot model =
    let
        idate =
            Array.get model.date_index model.insertion_dates
        ( start, end ) =
            getFetchBounds model.horizon
    in getdata
        { baseurl = model.baseurl
        , name = model.name
        , idate = idate
        , callback = GotPlotData
        , nocache = (U.bool2int model.horizon.viewNoCache)
        , fromdate = start
        , todate = end
        , horizon = Nothing
        , tzone = model.horizon.timeZone
        , inferredFreq = model.horizon.inferredFreq
        , keepnans = False
        , apipoint = "state"
        , exclude = "right"
        }


gethascache : Model -> Cmd Msg
gethascache model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-has-cache" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString HasCache
        }


deletecache : Model -> Cmd Msg
deletecache model =
    Http.request
        { method = "DELETE"
        , body = Http.jsonBody <| E.object
                 [ ("name", E.string model.name ) ]
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-has-cache" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString CacheDeleted
        }


getcachepolicy : String -> String -> Cmd Msg
getcachepolicy baseUrl name =
    Http.get
        { url =
              UB.crossOrigin
              baseUrl
              [ "api", "cache", "series-policy" ]
              [ UB.string "name" name ]
        , expect = Http.expectString GotCachePolicy
        }


getsomeidates : Model -> Cmd Msg
getsomeidates model =
    let
        bounds =
            getFromToDates model.horizon
        baseQuery =
            [ UB.string "name" model.name
            , UB.int "nocache" <| U.bool2int model.horizon.viewNoCache
            ]
        boundQuery =
            case bounds of
                Nothing ->
                    []
                Just ( min, max ) ->
                    [ UB.string "from_value_date" min
                    , UB.string "to_value_date" max
                    ]
    in Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "insertion_dates" ]
              ( baseQuery ++ boundQuery )
        , expect = Http.expectString HistoryIdates
        }


getVersions : Model -> List String -> Cmd Msg
getVersions model idates =
    let
        bounds =
            getFromToDates model.horizon
        baseQuery: String -> List UB.QueryParameter
        baseQuery idate =
            [ UB.string "name" model.name
            , UB.string "insertion_date" idate
            , UB.int "inferred_freq" ( U.bool2int model.horizon.inferredFreq )
            , UB.string "tzone" model.horizon.timeZone
            , UB.int "nocache" ( U.bool2int model.horizon.viewNoCache )
            ]
        boundQuery =
            case bounds of
                Nothing -> []
                Just ( min, max ) ->
                    [ UB.string "from_value_date" min
                    , UB.string "to_value_date" max
                    ]
        getVersion : String -> Cmd Msg
        getVersion idate =
            Http.get
                { url =
                      UB.crossOrigin
                      model.baseurl
                      [ "api", "series", "state" ]
                      ( (baseQuery idate) ++ boundQuery )
                , expect = Http.expectString (GotVersion model.integrity idate)
                }
    in
    Cmd.batch <| List.map getVersion idates


updatedchangedidatebouncer =
    { mapMsg = DebounceChangedIdate
    , getDebouncer = .date_index_deb
    , setDebouncer = \deb model -> { model | date_index_deb = deb }
    }


updatedChangedHistoryIdateDebouncer =
    { mapMsg = DebounceChangedHistoryIdate
    , getDebouncer = .historyDateIndexDeb
    , setDebouncer = \deb model -> { model | historyDateIndexDeb = deb }
    }


addError: Model -> String -> String -> Model
addError model tag error =
    U.adderror model (tag ++ " -> " ++ error)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror
                            ( setStatus model Failure )
                            (tag ++ " -> " ++ error)
    in
    case msg of

        Tab tab ->
            U.nocmd { model | activetab = tab }

        GotSysMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    let
                        isformula = Dict.member "formula" allmeta
                        model_horizon = model.horizon
                        newmodel =
                            { model
                                | meta = allmeta
                                , tzaware = (M.dget "tzaware" allmeta) == "true"
                                , timeseries =  if strseries allmeta
                                                    then SeriesString Dict.empty
                                                    else SeriesFloat Dict.empty
                                , seriestype = if isformula then I.Formula else I.Primary
                                , statistics = updateFirstLast
                                                    model.statistics
                                                    allmeta
                                , maxNbRevisions = if isformula
                                                    then Just defaultRevMaxFormula
                                                    else Just defaultRevMaxPrimary
                                , previousMax = if isformula
                                                    then defaultRevMaxFormula
                                                    else defaultRevMaxPrimary
                                , horizon = { model_horizon | hasCache = isformula }
                            }
                        cmd =
                            Cmd.batch <|
                            [ ]
                            ++ if isformula
                               then [ I.getformula
                                          model model.name model.formula_depth
                                          "series" GotFormula
                                    , getdepth model
                                    , gethascache model
                                    ]
                               else [ I.getlog model.baseurl model.name
                                          model.logsNumber "series" GotLog
                                    ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotSysMeta (Err err) ->
            let
                newmodel =
                    { model | errors =
                          List.append model.errors
                              [ ("gotsysmeta http" ++ " -> " ++ (U.unwraperror err)) ]
                    }
            in
            U.nocmd newmodel

        GotUserMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    U.nocmd { model | usermeta = allmeta }
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotUserMeta (Err err) ->
            doerr "gotusermeta http"  <| U.unwraperror err

        GotDepth (Ok rawdepth) ->
            let
                depth =
                    case D.decodeString (D.int) rawdepth of
                        Ok depth_ -> depth_
                        Err _ -> 0
            in
            ( { model | formula_maxdepth = depth }
            , Cmd.batch <|
                List.map
                    (\d -> I.getformula model model.name d "series" GotFormula)
                    <| List.range 0 depth
            )

        GotDepth (Err err) ->
            doerr "gotdepth http" <| U.unwraperror err

        GotSource (Ok rawsource) ->
            case D.decodeString D.string rawsource of
                Ok source ->
                    U.nocmd { model | source = source }
                Err err ->
                    doerr "gotsource decode" <| D.errorToString err

        GotSource (Err err) ->
            doerr "gotsource http" <| U.unwraperror err

        GetPermissions (Ok rawperm) ->
            case D.decodeString D.bool rawperm of
                Ok perms ->
                   U.nocmd { model | canwrite = perms }
                Err err ->
                    doerr "getpermissions decode" <| D.errorToString err

        GetPermissions (Err err) ->
            doerr "getpermissions http" <| U.unwraperror err

        GotPlotData (Ok rawdata) ->
            case decodeAndType rawdata of
                Ok series ->
                    if isEmpty series
                    then cleanView model series
                    else
                    let newmodel = { model
                                        | horizon = updateHorizonFromData
                                                        model.horizon
                                                        (getDates series)
                                        , timeseries = series
                                }
                    in
                    case series of
                        SeriesFloat ts ->
                            U.nocmd { newmodel |
                                         statistics = getStatistics
                                                model.statistics
                                                model.allowInferFreq
                                                ts
                                     }
                        SeriesString ts ->
                            U.nocmd newmodel

                Err (errFloat, errString) -> U.nocmd <|
                            addError
                            { model | horizon = setStatusPlot model.horizon Failure }
                            "decode and type"
                            <| ( D.errorToString errFloat ) ++ ( D.errorToString errString )

        GotPlotData (Err err) ->
            case err of
                Http.BadStatus code ->
                    if code == 404
                    then U.nocmd { model
                                     | doesnotexist = True
                                     , horizon = setStatusPlot model.horizon Failure
                                 }
                   else U.nocmd <|
                       addError
                       { model | horizon = setStatusPlot model.horizon Failure }
                       "gotplotdata decode"
                       ( U.unwraperror err )
                _ -> U.nocmd <|
                     addError
                     { model | horizon = setStatusPlot model.horizon Failure }
                     "gotplotdata decode"
                     ( U.unwraperror err )

        StatInfos sMsg ->
            case model.timeseries of
                SeriesString _ -> U.nocmd model
                SeriesFloat ts ->
                    case sMsg of
                        AllowInferFreq ->
                            ({ model | allowInferFreq = True
                                     , statistics = getStatistics
                                                        model.statistics
                                                        True
                                                        ts
                             }
                            , Cmd.none
                            )

        GotFormula (Ok rawformula) ->
            case D.decodeString I.formuladecoder rawformula of
                Ok resp ->
                   U.nocmd { model
                               | formula = Dict.insert resp.level resp.formula model.formula
                           }
                Err e ->
                    U.nocmd model

        GotFormula (Err error) ->
            doerr "gotformula http" <| U.unwraperror error

        SwitchLevel level ->
            let
                depth = Maybe.withDefault 0 <| String.toInt level
                newmodel = { model | formula_depth = depth }
            in
            U.nocmd newmodel

        -- cache

        HasCache (Ok rawhascache) ->
            let
                model_horizon = model.horizon
            in
            U.nocmd { model
                        | horizon =
                          { model_horizon
                              | hasCache = String.startsWith "true" rawhascache
                          }
                    }

        HasCache (Err error) ->
            doerr "hascache http" <| U.unwraperror error

        DeleteCache ->
            U.nocmd { model | deleting_cache = True }

        CacheConfirmDeletion ->
            ( { model | deleting_cache = False }
            , deletecache model
            )

        CacheCancelDeletion ->
            U.nocmd { model | deleting_cache = False }

        CacheDeleted (Ok _) ->
            let
                model_horizon =
                    model.horizon
                newmodel =
                    { model
                        | horizon =
                          { model_horizon | viewNoCache = False }
                    }
            in
            ( newmodel
            , Cmd.batch [ gethascache newmodel
                        , getplot newmodel
                        , I.getidates newmodel "series" InsertionDates model.horizon.viewNoCache
                        , I.getlog model.baseurl model.name model.logsNumber "series" GotLog
                        ]
            )

        CacheDeleted (Err error) ->
            doerr "cachedeleted http" <| U.unwraperror error

        GotCachePolicy (Ok rawpol) ->
            case rawpol of
                "null\n" -> U.nocmd model
                _ ->
                    case D.decodeString M.decodemeta rawpol of
                        Ok policy ->
                            U.nocmd { model | policy = policy }
                        Err err ->
                            doerr "gotcachepolicy decode" <| D.errorToString err

        GotCachePolicy (Err error) ->
            doerr "gotcachepolicy http" <| U.unwraperror error

        -- log

        GotLog (Ok rawlog) ->
            case D.decodeString I.logdecoder rawlog of
                Ok log ->
                    U.nocmd { model | log = log }
                Err err ->
                    doerr "gotlog decode" <| D.errorToString err

        GotLog (Err error) ->
            doerr "gotlog http" <| U.unwraperror error

        InsertionDates (Ok rawdates) ->
            case D.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    U.nocmd { model
                                | insertion_dates = Array.fromList dates
                                , date_index = List.length dates - 1
                            }
                Err err ->
                    doerr "idates decode" <| D.errorToString err

        InsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        DebounceChangedIdate val ->
            Debouncer.update update updatedchangedidatebouncer val model

        ChangedIdate strindex ->
            let
                index =
                    Maybe.withDefault
                    model.date_index -- keep current
                    (String.toInt strindex)
                newmodel =
                    { model | date_index = index }
            in
            case Array.get index model.insertion_dates of
                Nothing -> U.nocmd model
                Just date ->
                    ( setStatus newmodel Loading
                    , getplot newmodel
                    )

        IterIDate history direction ->
            if history
            then
            case direction of
                Prev -> U.nocmd { model |
                    historyDateIndex = ( model.historyDateIndex - 1 )}
                Next -> U.nocmd { model |
                    historyDateIndex = ( model.historyDateIndex + 1 )}
            else
            let newM = case direction of
                        Prev -> { model |
                            date_index = ( model.date_index - 1 )}
                        Next -> { model |
                            date_index = ( model.date_index + 1 )}
            in
                ( setStatus newM Loading
                , getplot newM
                )


        IdatePickerChanged value ->
            let
                comparedates d1 d2 =
                    d1 > d2
                newarray =
                    Array.filter (comparedates value) <|
                    Array.map U.cleanupdate model.insertion_dates
                newindex =
                    max 0 <| Array.length newarray - 1
                newmodel =
                    { model | date_index = newindex }
            in
            ( setStatus newmodel Loading
            , getplot newmodel
            )

        -- user metadata edition

        MetaEditAsked ->
            U.nocmd { model
                        | editing = True
                        , editeditems = Dict.map (\k v -> M.metavaltostring v) model.usermeta
                    }

        MetaEditCancel ->
            U.nocmd { model
                        | editing = False
                        , editeditems = Dict.empty
                        , metaitem = ("", "")
                    }

        MetaItemToDelete key ->
            U.nocmd { model | editeditems = Dict.remove key model.editeditems }

        EditedValue key value ->
            U.nocmd { model | editeditems = Dict.insert key value model.editeditems }

        NewKey key ->
            U.nocmd { model | metaitem = ( key, Tuple.second model.metaitem ) }

        NewValue val ->
            U.nocmd { model | metaitem = ( U.first model.metaitem, val ) }

        AddMetaItem ->
            -- eat the metaitems
            if (U.first model.metaitem == "") || (U.snd model.metaitem == "")
            then U.nocmd model else
            let
                edited = Dict.insert
                         (U.first model.metaitem)
                         (U.snd model.metaitem)
                         model.editeditems
            in
            U.nocmd { model
                        | metaitem = ("", "")
                        , editeditems = edited
                    }

        SaveMeta ->
            let
                decode rawitem =
                    case D.decodeString M.decodemetaval rawitem of
                        Ok item -> item
                        Err err ->
                            -- form strings are not json strings
                            -- this is why plain string parsing will fail ...
                            M.MString rawitem
                newmodel =
                    { model | usermeta = Dict.map (\k v -> decode v) model.editeditems }
            in
            ( newmodel
            , I.savemeta newmodel "series" MetaSaved
            )

        MetaSaved (Ok _) ->
            U.nocmd { model
                        | editing = False
                        , editeditems = Dict.empty
                        , metaitem = ("", "")
                    }

        MetaSaved (Err err) ->
            doerr "metasaved http" <| U.unwraperror err

        -- deletion

        AskDeletion ->
            U.nocmd { model | deleting = True }

        CancelDeletion ->
            U.nocmd { model | deleting = False }

        ConfirmDeletion ->
            ( model
            , I.delete model "series" Deleted
            )

        Deleted (Ok _) ->
            ( model
            , load <| UB.crossOrigin model.baseurl [ "tssearch" ] [ ]
            )

        Deleted (Err err) ->
            doerr "deletion failed" <| U.unwraperror err

        -- renaming

        AskRename ->
            U.nocmd { model | renaming = True }

        CancelRename ->
            U.nocmd { model
                        | renaming = False
                        , newname = Nothing
                    }

        EditNewName name ->
            U.nocmd { model | newname = Just name }

        ConfirmRename ->
            let
                cmd =
                    case model.newname of
                        Nothing -> Cmd.none
                        Just newname ->
                            I.rename model newname "series" Renamed
            in
            ( model
            , cmd
            )

        Renamed (Ok _) ->
            let name =
                    case model.newname of
                        Just newname -> newname
                        Nothing -> model.name
            in
            ( model
            , load <| UB.crossOrigin model.baseurl [ "tsinfo" ] [ UB.string "name" name ]
            )

        Renamed (Err err) ->
            doerr "renaming failed" <| U.unwraperror err

        CopyNameToClipboard ->
            ( { model | clipboardclass = "bi bi-check2" }
            , Cmd.batch
                [ copyToClipboard model.name
                , T.perform (always (ResetClipboardClass)) (P.sleep 1000)
                ]
            )

        ResetClipboardClass ->
            U.nocmd { model | clipboardclass = "bi bi-clipboard" }

        Horizon hmsg ->
            let
                ( newhorizonmodel, commands ) =
                    updateHorizon hmsg convertMsg model.horizon
                newmodel =
                    { model | horizon =  newhorizonmodel }
                default =
                    ( newmodel, commands )
                resetmodel =
                    { newmodel | historyPlots = Dict.empty
                    , dataFromHover = Nothing
                    , lastIdates = Array.empty
                    , nbRevisions = 0
                    }
                loadHistory =
                    [ T.succeed ( HistoryMode model.historyMode )
                    |> T.perform identity
                    ]
                idates = getIdates
                            newmodel.horizon
                            "series"
                            InsertionDates
                            model.name
            in
            case hmsg of
                HorizonModule.Fetch fetch ->
                    case fetch of
                        HorizonModule.GotBounds _ ->
                            ( newmodel
                            , Cmd.batch [ commands
                                        , getplot newmodel
                                        , idates
                                        ]
                            )

                        HorizonModule.GetDirectData _ ->
                            ( resetmodel
                            , Cmd.batch [ commands
                                        , getplot newmodel
                                        , idates
                                        ]
                            )

                        HorizonModule.Option op ->
                            case op of
                                HorizonModule.ViewNoCache ->
                                    ( { resetmodel | insertion_dates = Array.empty }
                                    , Cmd.batch ([ commands
                                                 , getplot newmodel
                                                 , idates
                                                 ]
                                                 ++ loadHistory
                                                 )
                                    )
                                _ -> ( resetmodel
                                     , Cmd.batch ([ commands
                                                  , getplot newmodel
                                                  ]
                                                 ++ loadHistory
                                                )
                                     )

                HorizonModule.Frame _ ->
                    ( resetmodel
                    , commands
                    )

                _ -> default

        HistoryMode isChecked ->
            let
                newmodel =
                    { model
                        | historyMode = isChecked
                        , historyPlots = Dict.empty
                        , lastIdates = Array.empty
                        , dataFromHover = Nothing
                        , nbRevisions = 0
                    }
            in
            if isChecked
            then
                case model.horizon.zoomBounds of
                    Nothing ->
                        U.nocmd newmodel
                    Just _ ->
                        ( newmodel
                        , getsomeidates newmodel
                        )
             else ( { newmodel | wipe = True }
                  , T.perform
                        (always Transitory)
                         (P.sleep 10)
                 )
        Transitory ->
            U.nocmd { model | wipe = False }

        FromZoom zoom ->
            let
                horizonmodel = model.horizon
                newZoom = updateZoom model.horizon ( extractZoomDates zoom )
                rangeX = newZoom.x
                integrity = model.integrity
            in
            if model.historyMode
            then
                case rangeX of
                    Nothing ->
                        U.nocmd { model
                                    | historyPlots = Dict.empty
                                    , lastIdates = Array.empty
                                    , dataFromHover = Nothing
                                    , integrity = integrity + 1
                                    , nbRevisions = 0
                                    , horizon = { horizonmodel |
                                                      zoomBounds = Nothing
                                                }
                                }
                    Just ( minDate, maxDate ) ->
                        let newmodel =
                                { model
                                    | historyPlots = Dict.empty
                                    , lastIdates = Array.empty
                                    , dataFromHover = Nothing
                                    , integrity = integrity + 1
                                    , horizon = { horizonmodel |
                                                      zoomBounds = Just ( minDate, maxDate )
                                                }
                                }
                        in ( newmodel, getsomeidates newmodel )

            else
                let newmodel = { model
                                | horizon =
                                  { horizonmodel
                                      | zoomBounds = newZoom.x
                                      , zoomY = newZoom.y
                                  }
                               }
                in
                case model.timeseries of
                    SeriesString _ -> U.nocmd newmodel
                    SeriesFloat ts ->
                        U.nocmd
                            { newmodel |
                                statistics = getStatistics
                                                model.statistics
                                                model.allowInferFreq
                                                <| applyZoom
                                                    ts
                                                    newZoom.x
                            }

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }

        HistoryIdates (Ok rawdates) ->
            case D.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    let
                        nbRevisions =
                            List.length dates
                        lasts =
                            lastDates dates ( Maybe.withDefault 0 model.maxNbRevisions )
                        newmodel =
                            { model
                                | lastIdates = Array.fromList lasts
                                , historyIdates = Array.fromList dates
                                , historyDateIndex = List.length lasts - 1
                                , nbRevisions = nbRevisions
                            }
                    in
                    ( newmodel
                    , getVersions model lasts
                    )
                Err err ->
                    doerr "idates decode" <| D.errorToString err

        HistoryIdates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        GotVersion when idate (Ok rawdata) ->
            if when /= model.integrity
            then U.nocmd model
            else
            case D.decodeString seriesdecoder rawdata of
                Ok val ->
                    let
                        newHistoryPlots = Dict.insert idate val model.historyPlots
                    in
                    U.nocmd { model | historyPlots = newHistoryPlots }
                Err err ->
                    if strseries model.meta
                    then U.nocmd model
                    else doerr "gotplotdata decode" <| D.errorToString err

        GotVersion _ _ (Err err) ->
            doerr "gotplotdata error" <| U.unwraperror err

        DebounceChangedHistoryIdate val ->
            Debouncer.update update updatedChangedHistoryIdateDebouncer val model

        ChangedHistoryIdate strindex ->
            let
                index =
                    Maybe.withDefault model.historyDateIndex -- keep current
                    ( String.toInt strindex )
                newmodel =
                    if Array.get index model.lastIdates == Nothing
                    then
                        model
                    else
                        { model
                            | historyDateIndex = index
                            , dataFromHover = Nothing
                        }
            in U.nocmd newmodel

        ViewAllHistory ->
            let integrity = model.integrity
            in
            ( { model
                  | historyPlots = Dict.empty
                  , lastIdates = Array.empty
                  , integrity = integrity
              }
            , getsomeidates model
            )

        ChangeMaxRevs newMax ->
            case String.toInt newMax of
                Nothing ->
                    U.nocmd { model | maxNbRevisions = Nothing }
                Just max ->
                    U.nocmd { model | maxNbRevisions = Just max }

        UpdateMax ->
            case model.maxNbRevisions of
                Nothing ->
                    U.nocmd model
                Just max ->
                    let
                        lasts =
                            lastDates
                            ( Array.toList model.historyIdates )
                            ( Maybe.withDefault 0 model.maxNbRevisions )
                        newmodel =
                            { model
                                | historyPlots = Dict.empty
                                , dataFromHover = Nothing
                                , previousMax = max
                                , lastIdates = Array.fromList lasts
                                , historyDateIndex = min (max - 1) ( List.length lasts - 1)
                            }
                    in ( newmodel , getVersions model lasts )

        NewDataFromHover data ->
            case D.decodeString dataFromHoverDecoder data of
                Ok datadict ->
                    U.nocmd { model | dataFromHover = Just (removeRedundants datadict) }
                Err _ ->
                    U.nocmd model

        LogsNumber logcount ->
            U.nocmd { model | logsNumber = String.toInt logcount }

        SeeLogs ->
            ( model
            , I.getlog model.baseurl model.name model.logsNumber "series" GotLog
            )


lastDates: List String -> Int -> List String
lastDates dates max =
     if (List.length dates) > max
     then
        List.reverse ( List.take max ( List.reverse dates ))
     else
        dates


getDates: TimeSeries -> List String
getDates series =
    case series of
        SeriesFloat ts -> Dict.keys ts
        SeriesString ts -> Dict.keys ts


applyZoom: Dict String a ->  Maybe (String, String) -> Dict String a
applyZoom series bounds =
    case bounds of
        Nothing -> series
        Just ( min, max ) ->
           Dict.filter
                (( \k _ -> (( k >= min ) && ( k <= max ))))
                series

-- views

viewcachepolicy : Model -> H.Html Msg
viewcachepolicy model =
    let
        elt name =
            H.li [] [ H.text <| name
                          ++ " → "
                          ++ (M.dget name model.policy)
                    ]
    in
    H.div [ ]
        [ H.h2 [ ] [ H.text "Policy" ]
        , H.ul [ HA.class "highlight" ] <|
            List.map elt [ "name"
                         , "initial_revdate"
                         , "look_before"
                         , "look_after"
                         , "revdate_rule"
                         , "schedule_rule"
                         ]
        ]

viewcache : Model -> H.Html Msg
viewcache model =
    let
        cachecontrol =
            H.span [ ]
                [ if List.length model.log > 0
                  then I.viewlog model False LogsNumber SeeLogs
                  else H.span [] []
                , if Dict.isEmpty model.policy
                  then H.span [] []
                  else viewcachepolicy model
                ]

        deleteaction =
            if model.horizon.hasCache then
                if model.deleting_cache then
                    H.span [ ]
                        [ H.button
                              [ HA.class "btn btn-warning"
                              , HA.attribute "type" "button"
                              , HE.onClick CacheCancelDeletion ]
                              [ H.text "cancel" ]
                        , H.span [] [ H.text " " ]
                        , H.button
                            [ HA.class "btn btn-danger"
                            , HA.attribute "type" "button"
                            , HE.onClick CacheConfirmDeletion ]
                            [ H.text "confirm" ]
                        ]
                else
                    H.button
                        [ HA.class "btn btn-danger"
                        , HA.attribute "type" "button"
                        , HA.title "This is an irreversible operation."
                        , HE.onClick DeleteCache ]
                        [ H.text "delete" ]
            else
                H.span [] []

    in
    case model.seriestype of
        I.Formula ->
            H.div []
                [ H.h2
                      []
                      [ H.text "Cache"
                      , H.span [] [ H.text " " ]
                      , deleteaction
                      ]
                , if model.horizon.hasCache then
                      H.span [] []
                  else
                      H.div [] [ H.text "There is no cache yet." ]
                , cachecontrol
                ]
        I.Primary ->
            H.div [] []







extractMax max =
    case max of
        Nothing -> ""
        Just nb -> String.fromInt nb


historyInput: Model -> H.Html Msg
historyInput model =
    if model.nbRevisions > 0
    then
        H.span [ ]
            [ H.text ( "   " ++
                       (String.fromInt model.nbRevisions) ++
                       " revisions. Only showing the last "
                     )
            , H.input
                [ HA.value ( extractMax model.maxNbRevisions )
                , HA.class "form-control-sm"
                , HA.attribute "type" "text"
                , HA.style "width" "4em"
                , HE.onInput ChangeMaxRevs ]
                [ ]
            ]
    else H.div [] []


plotString: Model -> Dict String String -> H.Html Msg
plotString model ts =
    let pseudoTs = Dict.map
                    (\ _ v -> if v /= ""
                                then Just 1
                                else Just 0
                    )
                    ts
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
                        (Dict.keys pseudoTs)
                        (Dict.values pseudoTs)
                        "markers"
                        { defaultTraceOptions
                            | text = Just ( Dict.values ts )
                            , hoverinfo = Just "text"
                        }
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


isEmpty: TimeSeries -> Bool
isEmpty series =
    case series of
        SeriesFloat ts -> Dict.isEmpty ts
        SeriesString ts -> Dict.isEmpty ts


cleanView: Model -> TimeSeries -> (Model, Cmd Msg)
cleanView model series =
    ( { model | timeseries = series
              , wipe = True
      }
    , T.perform
        (always Transitory)
         (P.sleep 10)
    )


viewplot : Model -> Dict String (Maybe Float) -> H.Html Msg
viewplot model ts =
    if model.wipe
    then H.div [] []
    else
    let
        defaultLayout =
            { defaultLayoutOptions
                | xaxis = { defaultDateAxis
                              | range = extractDates model.horizon.zoomBounds
                          }
                , yaxis = { defaultValueAxis
                              | range = extractValues model.horizon.zoomY
                          }
                , dragMode = Just ( if model.panActive then "pan" else "zoom" )
            }
    in
    if model.historyMode
    then
        H.div []
            [ historyModeSwitch model
            , H.div
                [ ]
                [ H.text "Zoom to select a range or "
                , H.button
                    [ HA.class "btn btn-warning btn-sm"
                    , HA.attribute "type" "button"
                    , HE.onClick ViewAllHistory
                    , HA.disabled ( Array.length model.lastIdates /= 0 )
                    ]
                    [ H.text "view all history" ]
                , historyInput model
                , H.button
                    [ HA.class "btn btn-primary btn-sm"
                    , HA.attribute "type" "button"
                    , HE.onClick UpdateMax
                    , HA.hidden (( model.maxNbRevisions == Just model.previousMax )
                                 || ( model.maxNbRevisions == Nothing ))
                    ]
                    [ H.text "Submit" ]
                ]
            , H.div
                [ HA.class "under-the-header"]
                [ H.div
                    [ HA.class "plot-and-stuffs" ]
                    [ I.viewgraph
                        (Dict.fromList [ (model.name, ts) ])
                        defaultLayout
                        defaultTraceOptions
                        model.horizon.inferredFreq
                    ]
                , H.div
                    [ HA.class "stat-table-container"]
                    [ viewStatTable
                        model.statistics
                        model.roundStat
                        convertStat
                    ]
                ]
            , I.viewDatesRange
                model.lastIdates
                model.historyDateIndex
                DebounceChangedHistoryIdate
                ChangedHistoryIdate
            , I.viewWidgetIdates
                model.historyMode
                model.lastIdates
                model.historyDateIndex
                IterIDate
            , I.viewHistoryGraph model
            , if Array.isEmpty model.lastIdates then
                  H.div
                      [ HA.class "placeholder-text-hover" ]
                      [ ]
              else
                H.div
                    [ ]
                    [ H.text """Place the mouse on the graph above to see
                             the versions of one application date: """
                    ]
            , if model.debug
              then H.div [ ] [ showHoverData model ]
              else H.div [ ] [ ]
            , case model.dataFromHover of
                  Just data ->
                      I.viewHoverGraph data
                  Nothing ->
                      I.viewHoverGraph { name = ""
                                       , data = []
                                       }
            ]
    else
        H.div []
            [ historyModeSwitch model
            , I.viewDatesRange
                model.insertion_dates
                model.date_index
                DebounceChangedIdate
                ChangedIdate
            , I.viewWidgetIdates
                model.historyMode
                model.insertion_dates
                model.date_index
                IterIDate
            , H.div
                [ HA.class "under-the-header"]
                [ H.div
                    [ HA.class "plot-and-stuffs" ]
                    [ I.viewgraph
                        (Dict.fromList [ (model.name, ts) ])
                        defaultLayout
                        defaultTraceOptions
                        model.horizon.inferredFreq
                    ]
                , H.div
                    [ HA.class "stat-table-container"]
                    [ viewStatTable
                        model.statistics
                        model.roundStat
                        convertStat
                    ]
                ]
            ]


historyModeSwitch : Model -> H.Html Msg
historyModeSwitch model =
    H.div
        [ HA.class "custom-control custom-switch"]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "historyModeCheckDefault"
            , HA.checked model.historyMode
            , HE.onCheck HistoryMode
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "historyModeCheckDefault"
            ]
            [ H.text "History mode" ]
        ]


showHoverData: Model -> H.Html Msg
showHoverData model =
     case model.dataFromHover of
         Nothing -> H.text "Hover-data : Nothing"
         Just data -> H.text ( "Hover-data, name : " ++ data.name )


viewstrseries model ts =
    let
        tsrow (stamp, value) =
            H.tr [ ]
                [ H.td [ ] [ H.text stamp ]
                , H.td [ ] [ H.text value ]
                ]
        restricted = applyZoom ts model.horizon.zoomBounds
        showseries =
            List.map tsrow <| Dict.toList restricted

    in
    H.div [ ]
        [ I.viewDatesRange
              model.insertion_dates
              model.date_index
              DebounceChangedIdate
              ChangedIdate
        , I.viewWidgetIdates
              model.historyMode
              model.insertion_dates
              model.date_index
              IterIDate
        , plotString model ts
        , H.table [ HA.class "table w-auto" ]
              [ H.thead [ ]
                    [ H.tr [ ]
                          [ H.th [ ] [ H.text "Dates" ]
                          , H.th [ ] [ H.text "Values" ]
                          ]
                    ]
              , H.tbody [ ]
                  showseries
              ]
        ]


view : Model -> H.Html Msg
view model =
    let
        tablist =
            case model.seriestype of
                I.Primary ->
                    [ Plot, Metadata, Logs ]
                I.Formula ->
                    [ Plot, Metadata, FormulaCache ]

        tabs =
            tablist
                |> LS.fromList
                |> LS.select model.activetab

        head =
            header Tab tabs

        deleteEvents =
            DeleteEvents
            ConfirmDeletion
            CancelDeletion
            AskDeletion

        renameEvents =
            RenameEvents
            ConfirmRename
            EditNewName
            CancelRename
            AskRename

        metaEvents =
            MetaEvents
            MetaEditAsked
            MetaEditCancel
            EditedValue
            MetaItemToDelete
            NewKey
            NewValue
            SaveMeta
            AddMetaItem

        viewtabs =
            [ case model.activetab of
                  Plot ->
                      case model.timeseries of
                          SeriesString ts ->
                            H.div []
                              [ head
                              , tabcontents
                                    [ viewstrseries model ts ]
                              ]
                          SeriesFloat ts ->
                            H.div
                              [ ]
                              [ head
                              , tabcontents
                                    [ viewplot model ts
                                    , I.viewformula model SwitchLevel
                                    ]
                              ]

                  Metadata ->
                      H.div
                          []
                          [ head
                          , tabcontents [ I.viewusermeta model metaEvents False ]
                          ]

                  Logs ->
                      H.div []
                          [ head
                          , tabcontents
                                [ case model.seriestype of
                                      I.Primary ->
                                          I.viewlog model False LogsNumber SeeLogs
                                      I.Formula ->
                                          H.span [] []
                                ]
                          ]

                  FormulaCache ->
                      H.div [] [ head, tabcontents [ viewcache model ] ]

            , I.viewerrors model
            ]

    in
    H.div
        [ ]
        [ H.div
              [ HA.class "main-content"
              , HA.class "tsinfo"
              ]
              [ H.div
                [ ]
                ( [ H.span [ HA.class "tsinfo action-container" ]
                        <| (I.viewactionwidgets
                            model
                            (I.SeriesType model.seriestype)
                            convertMsg
                            Nothing
                            True
                            "Series Info"
                            ( getFromToDates model.horizon )
                           ) ++
                        [ I.viewdeletion model deleteEvents
                        , I.viewrenameaction model renameEvents
                        ]
                  , I.viewtitle model model.clipboardclass CopyNameToClipboard
                  ] ++
                      if model.doesnotexist
                      then
                          [ I.msgdoesnotexist "Series"]
                      else viewtabs
                )
              ]
        ]


type alias Input =
    { baseurl : String
    , name : String
    , min: String
    , max: String
    , debug: String
    }


init: Input -> ( Model, Cmd Msg)
init input =
    let
        debouncerconfig =
            Debouncer.manual
                |> settleWhenQuietFor (Just <| fromSeconds 0.015)
                |> toDebouncer
    in
    ( { baseurl = input.baseurl
      , name = input.name
      , source = ""
      , tzaware = True
      , activetab = Plot
      -- metadata edition
      , canwrite = False
      , editing = False
      -- all errors
      , errors = [ ]
      , doesnotexist= False
      -- metadata
      , meta = Dict.empty
      , usermeta = Dict.empty
      , seriestype = I.Primary
      , timeseries = SeriesFloat Dict.empty
      , statistics = emptyStat
      , allowInferFreq = False
      , roundStat= 2
      -- formula
      , formula_depth = 0
      , formula_maxdepth = 0
      , formula = Dict.empty
      -- cache
      , policy = Dict.empty
      , deleting_cache = False
      -- log
      , log = [ ]
      , logsNumber = Just 10
      -- plot
      , insertion_dates = Array.empty
      , date_index = 0
      , date_index_deb = debouncerconfig
      , panActive = False
      -- user meta edition
      , metaitem = ("", "")
      , editeditems = Dict.empty
      -- deletion
      , deleting = False
      -- renaming
      , renaming = False
      , newname = Nothing
      , clipboardclass = "bi bi-clipboard"
      , horizon = initHorizon
                    input.baseurl
                    input.min
                    input.max
                    input.debug
                    Loading
      , wipe = False
      , integrity = 0
      , historyPlots = Dict.empty
      , historyMode = False
      , historyIdates = Array.empty
      , lastIdates = Array.empty
      , historyDateIndex = 0
      , nbRevisions = 0
      , maxNbRevisions = Just 0
      , previousMax = 0
      , historyDateIndexDeb = debouncerconfig
      , dataFromHover = Nothing
      , debug = False
      }
    , Cmd.batch
        [ M.getsysmetadata input.baseurl input.name GotSysMeta "series"
        , M.getusermetadata input.baseurl input.name GotUserMeta "series"
        , getsource input.baseurl input.name
        , I.getwriteperms input.baseurl GetPermissions
        , getcachepolicy input.baseurl input.name
        ]
    )


main : Program Input  Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ -> Sub.batch
                  [ zoomPlot FromZoom
                  , dataFromHover NewDataFromHover
                  , panActive NewDragMode
                  , loadFromLocalStorage
                        (\ s -> convertMsg (HorizonModule.FromLocalStorage s))
                  ]
        }
