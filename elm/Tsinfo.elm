port module Tsinfo exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation exposing (load)
import Debouncer.Messages as Debouncer exposing
    (Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Either exposing (Either(..))
import Horizon exposing
    ( DataInCache
    , Horizon
    , HorizonModel
    , Offset
    , dataInCacheDecoder
    , defaultHorizon
    , horizons
    , horizonwidget
    , saveToLocalStorage
    , savedDataInCache
    , updateDataInCache
    , updateHorizon
    , updateHorizonModel
    , updateOffset
    )
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Info as I
import Info exposing (SeriesType(..))
import Json.Decode as D
import Json.Encode as E
import JsonTree as JT exposing (TaggedValue(..))
import List.Selection as LS
import Maybe.Extra as Maybe
import Metadata as M
import OrderedDict as OD
import Plotter exposing
    ( getData
    , seriesdecoder
    , scatterplot
    , plotargs
    )
import Process as P
import Task as T
import Url.Builder as UB
import Util as U


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : M.UserMetadata
    }


type alias IdatePickerEvents =
    { idatepickerchanged : String -> Msg
    , fvdatepickerchanged : String -> Msg
    , tvdatepickerchanged : String -> Msg
    }


type PlotStatus
    = Loading
    | Success
    | Failure


type Tabs
    = Plot
    | Logs
    | UserMetadata
    | FormulaCache


type alias Model =
    { baseurl : String
    , name : String
    , source : String
    , activetab : Tabs
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    -- metadata, ventilated by std (system) and user
    , meta : M.StdMetadata
    , usermeta : M.UserMetadata
    , seriestype : I.SeriesType
    -- formula
    , formula_depth : Int
    , formula_maxdepth : Int
    , formula : Dict Int String
    -- cache
    , has_cache : Bool
    , view_nocache : Bool
    , policy : M.StdMetadata
    , deleting_cache : Bool
    -- log
    , log : List Logentry
    -- plot
    , insertion_dates : Array String
    , date_index : Int
    , date_index_deb : Debouncer Msg
    -- user meta edition
    , metaitem : (String, String)
    , editeditems : Dict String String
    -- deletion
    , deleting : Bool
    -- renaming
    , renaming : Bool
    , newname : Maybe String
    , clipboardclass : String
    , horizon : HorizonModel (Maybe Float)
    , plotStatus : PlotStatus
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
    -- dates
    | ChangedIdate String
    | DebounceChangedIdate (Debouncer.Msg Msg)
    | IdatePickerChanged String
    | FvdatePickerChanged String
    | TvdatePickerChanged String
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
    | ViewNocache
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
    | HorizonSelected Horizon
    | UpdateOffset Offset
    | SetDataInCache String
    | TimeZoneSelected String
    | InferredFreq Bool


logentrydecoder : D.Decoder Logentry
logentrydecoder =
    D.map4 Logentry
        (D.field "rev" D.int)
        (D.field "author" D.string)
        (D.field "date" D.string)
        (D.field "meta" (D.dict M.decodemetaval))


logdecoder : D.Decoder (List Logentry)
logdecoder =
    D.list logentrydecoder


getdepth : Model -> Cmd Msg
getdepth model =
    Http.get
        { expect = Http.expectString GotDepth
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "formula_depth" ]
              [ UB.string "name" model.name ]
        }


getsource : Model -> String -> Cmd Msg
getsource model name =
    Http.get
        { expect = Http.expectString GotSource
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "source" ]
              [ UB.string "name" name ]
        }


getplot : Model -> Bool -> Cmd Msg
getplot model atidate =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getData
            { baseurl = model.baseurl
            , name = model.name
            , idate = (if atidate then idate else Nothing)
            , callback = GotPlotData
            , nocache = (U.bool2int model.view_nocache)
            , fromdate = Maybe.unwrap
                "" (always model.horizon.mindate) model.horizon.horizon.key
            , todate = Maybe.unwrap
                "" (always model.horizon.maxdate) model.horizon.horizon.key
            , horizon = model.horizon.horizon.key
                |> Maybe.andThen (\key-> OD.get key horizons)
                |> Maybe.map
                    (String.replace "{offset}" (String.fromInt model.horizon.offset))
            , tzone = model.horizon.timeZone
            , inferredFreq = model.horizon.inferredFreq
            , keepnans = False
           }
           "state"


getlog : String -> String-> Cmd Msg
getlog urlprefix name  =
    Http.get
        { expect = Http.expectString GotLog
        , url = UB.crossOrigin urlprefix
              [ "api", "series", "log" ]
              [ UB.string "name" name
              , UB.int "limit" 10 ]
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


getcachepolicy : Model -> Cmd Msg
getcachepolicy model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-policy" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString GotCachePolicy
        }


updatedchangedidatebouncer =
    { mapMsg = DebounceChangedIdate
    , getDebouncer = .date_index_deb
    , setDebouncer = \deb model -> { model | date_index_deb = deb }
    }


strseries : Model -> Bool
strseries model =
    case M.dget "value_type" model.meta of
        "object" -> True
        _ -> False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        Tab tab ->
            U.nocmd { model | activetab = tab }

        GotSysMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    let
                        isformula = Dict.member "formula" allmeta
                        newmodel =
                            { model
                                | meta = allmeta
                                , seriestype = if isformula then I.Formula else I.Primary
                            }
                        cmd = Cmd.batch <| [ I.getidates model "series" InsertionDates ]
                              ++ if isformula
                                 then [ I.getformula
                                            model model.name model.formula_depth
                                            "series" GotFormula
                                      , getdepth model
                                      , gethascache model
                                      ]
                                 else [ getlog model.baseurl model.name ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotSysMeta (Err err) ->
            let
                newmodel =
                    { model
                        | errors = List.append model.errors
                          [("gotsysmeta http" ++ " -> " ++ (U.unwraperror err))]
                        , plotStatus = Failure
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
            , Cmd.batch (List.map
                             (\d -> I.getformula model model.name d "series" GotFormula)
                             <| List.range 0 depth
                        )
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
            doerr "gotsource http"  <| U.unwraperror err

        GetPermissions (Ok rawperm) ->
            case D.decodeString D.bool rawperm of
                Ok perms ->
                   U.nocmd { model | canwrite = perms }
                Err err ->
                    doerr "getpermissions decode" <| D.errorToString err

        GetPermissions (Err err) ->
            doerr "getpermissions http" <| U.unwraperror err

        GotPlotData (Ok rawdata) ->
            case D.decodeString seriesdecoder rawdata of
                Ok val ->
                    let
                        newmodel =
                            { model
                                | horizon = updateHorizonModel model.horizon val
                                , plotStatus = Success
                            }
                    in
                    U.nocmd newmodel
                Err err ->
                    if strseries model
                    then U.nocmd model
                    else doerr "gotplotdata decode" <| D.errorToString err

        GotPlotData (Err err) ->
            doerr "gotplotdata error" <| U.unwraperror err

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
            U.nocmd { model | has_cache = String.startsWith "true" rawhascache }

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
            let newmodel = { model | view_nocache = False } in
            ( newmodel
            , Cmd.batch [ gethascache newmodel
                        , getplot newmodel False
                        , I.getidates newmodel "series" InsertionDates
                        , getlog model.baseurl model.name
                        ]
            )

        CacheDeleted (Err error) ->
            doerr "cachedeleted http" <| U.unwraperror error

        ViewNocache ->
            let
                mod = { model
                    | view_nocache = not model.view_nocache
                    , plotStatus = Loading
                    }
            in
            ( mod
            , Cmd.batch
                [ I.getidates mod "series" InsertionDates
                , getplot mod False
                ]
            )

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
            case D.decodeString logdecoder rawlog of
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
                index = Maybe.withDefault
                       model.date_index -- keep current
                       (String.toInt strindex)
                newmodel = { model | date_index = index }
            in
            case Array.get index model.insertion_dates of
                Nothing -> U.nocmd model
                Just date ->
                    ( newmodel
                    , getplot newmodel True
                    )

        IdatePickerChanged value ->
            let
                comparedates d1 d2 =
                    d1 > d2
                newarray =  Array.filter (comparedates value) <|
                            Array.map U.cleanupdate model.insertion_dates
                newindex = max 0 <| Array.length newarray - 1
                newmodel = { model | date_index = newindex }
            in
            ( newmodel
            , getplot newmodel True
            )

        FvdatePickerChanged value ->
            let
                newHorizonModel = model.horizon
                newmodel =
                    { model | horizon =
                          { newHorizonModel | mindate = value }
                    }
            in
            ( newmodel
            , getplot newmodel True
            )

        TvdatePickerChanged value ->
            let
                newHorizonModel = model.horizon
                newmodel = { model | horizon =
                                 { newHorizonModel | maxdate = value }
                           }
            in
            ( newmodel
            , getplot newmodel True
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
                newmodel = { model | usermeta = Dict.map (\k v -> decode v) model.editeditems }
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
            doerr "deletion failed" <| U.unwraperror err

        CopyNameToClipboard ->
            ( { model | clipboardclass = "bi bi-check2" }
            , Cmd.batch
                [ copyToClipboard model.name
                , T.perform (always (ResetClipboardClass)) (P.sleep 1000)
                ]
            )

        ResetClipboardClass ->
            U.nocmd { model | clipboardclass = "bi bi-clipboard" }

        HorizonSelected horizon ->
            let
                dataInCache =
                    DataInCache
                        horizon.key
                        model.horizon.timeZone
                        model.horizon.inferredFreq

                newmodel = { model | horizon = updateHorizon horizon model.horizon }
            in
            ( newmodel
            , Cmd.batch
                [ getplot newmodel False
                , saveToLocalStorage dataInCache
                ]
            )

        UpdateOffset (Left i) ->
            updateModelOffset model i

        UpdateOffset (Right i) ->
            updateModelOffset model -i

        SetDataInCache newDataInCache ->
            case D.decodeString dataInCacheDecoder newDataInCache of
                Ok newDataInCacheDict ->
                    let
                        newmodel =
                            { model | horizon =
                                  updateDataInCache newDataInCacheDict model.horizon
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                        [ getplot newmodel False
                        , saveToLocalStorage newDataInCacheDict
                        ]
                    )
                Err _ ->
                    (model, getplot model False)

        TimeZoneSelected timeZone ->
            let
                horizon = model.horizon
                newmodel =
                    { model |
                          horizon = { horizon | timeZone = timeZone }
                    }
                dataInCache =
                    DataInCache
                        model.horizon.horizon.key
                        timeZone
                        model.horizon.inferredFreq

            in
            ( newmodel
            , Cmd.batch
                [ getplot model False
                , I.getidates newmodel "series" InsertionDates
                , saveToLocalStorage dataInCache
                ]
            )

        InferredFreq isChecked ->
            let
                horizon = model.horizon

                newmodel =
                    { model | horizon = { horizon | inferredFreq = isChecked } }

                dataInCache =
                    DataInCache
                        model.horizon.horizon.key
                        model.horizon.timeZone
                        isChecked
            in
            ( newmodel
            , Cmd.batch
                [ getplot model False
                , saveToLocalStorage dataInCache
                ]
            )


-- views

updateModelOffset : Model -> Int -> (Model, Cmd Msg)
updateModelOffset model i =
    let
        offset =
            (model.horizon.offset + i)
        newmodel =
            { model | horizon = updateOffset offset model.horizon }
    in
    ( newmodel
    , getplot newmodel False
    )


port copyToClipboard : String -> Cmd msg


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


viewtogglecached : Model -> H.Html Msg
viewtogglecached model =
    let
        title =
            if model.plotStatus == Loading then
                "Loading ..."
            else if model.view_nocache then
                "view cached"
            else
                "view uncached"

    in
    H.div
        [ HA.class "custom-control custom-switch"
        , HA.title title
        ]
        [ H.input
              [ HA.attribute "type" "checkbox"
              , HA.class "custom-control-input"
              , HA.id "view-uncached"
              , HA.checked <| not model.view_nocache
              , HE.onClick ViewNocache
              , HA.disabled (model.plotStatus == Loading)
              ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "view-uncached"
            ]
            [ H.text title ]
        ]


viewcache : Model -> H.Html Msg
viewcache model =
    let
        cachecontrol =
            H.span [ ]
                [ if List.length model.log > 0
                  then I.viewlog model False
                  else H.span [] []
                , if Dict.isEmpty model.policy
                  then H.span [] []
                  else viewcachepolicy model
                , if model.has_cache
                  then viewtogglecached model
                  else H.span [] []
                ]

        deleteaction =
            if model.has_cache then
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
                , if model.has_cache then
                      H.span [] []
                  else
                      H.div [] [ H.text "There is no cache yet." ]
                , cachecontrol
                ]
        I.Primary ->
            H.div [] []


viewdatesrange : Model -> H.Html Msg
viewdatesrange model =
    let
        numidates = Array.length model.insertion_dates
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> date
    in
    if numidates < 2
    then H.div [] []
    else
        H.map (provideInput >> DebounceChangedIdate) <|
            H.div []
            [ H.input
                  [ HA.attribute "type" "range"
                  , HA.min "0"
                  , HA.max (String.fromInt (numidates - 1))
                  , HA.value (String.fromInt model.date_index)
                  , HA.class "form-control-range"
                  , HA.title currdate
                  , HE.onInput ChangedIdate
                  ] [ ]
            ]


idatepickerevents : IdatePickerEvents
idatepickerevents =
    { idatepickerchanged = IdatePickerChanged
    , fvdatepickerchanged = FvdatePickerChanged
    , tvdatepickerchanged = TvdatePickerChanged
    }


viewplot : Model -> H.Html Msg
viewplot model =
    let
        plot = scatterplot model.name
               (Dict.keys model.horizon.timeSeries)
               (Dict.values model.horizon.timeSeries)
               "lines"
        args = plotargs "plot" [plot]
    in
    H.div []
        [ viewdatespicker model idatepickerevents
        , viewdatesrange model
        , H.div [ HA.id "plot" ] []
        -- the "plot-figure" node is pre-built in the template side
        -- (html component)
        , H.node "plot-figure" [ HA.attribute "args" args ] []
        ]


metaevents =
    { metaeditasked = MetaEditAsked
    , metaeditcancel = MetaEditCancel
    , editedvalue = EditedValue
    , metaitemtodelete = MetaItemToDelete
    , newkey = NewKey
    , newvalue = NewValue
    , savemeta = SaveMeta
    , addmetaitem = AddMetaItem
    }


deleteevents =
    { confirmdeletion = ConfirmDeletion
    , canceldeletion = CancelDeletion
    , askdeletion = AskDeletion
    }


renameevents =
    { confirmrename = ConfirmRename
    , editnewname = EditNewName
    , cancelrename = CancelRename
    , askrename = AskRename
    }



viewdatespicker : Model ->  IdatePickerEvents -> H.Html Msg
viewdatespicker model events =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date
    in H.div
        [ HA.class "row" ]
        [H.div
            [ HA.class "col" ]
            [ H.label [ HA.for "idate-picker" ] [ H.text "Revision date : " ]
            , H.span [ ] [ H.text " " ]
            , H.input [ HA.type_ "datetime-local"
                        , HA.id "idate-picker"
                        , HA.name "idate-picker"
                        , HA.value currdate
                        , HE.onInput events.idatepickerchanged
                        ] [ ]
            ]
        ]


viewactionwidgets model =
    let
        editorlabel =
            case model.seriestype of
                Primary ->  "edit values ⧉"
                Formula ->  "show values ⧉"
    in
    [ H.div
          [ HA.style "color" "grey", HA.class "title-like action-left" ]
          [ H.text "Series " ]
    , horizonwidget
        model.horizon
            { inferredFreqMsg = InferredFreq
            , timeZoneMsg = TimeZoneSelected
            , offsetMsg = UpdateOffset
            , timeDeltaMsg = HorizonSelected
            }
        "action-center"
    , H.div [ HA.class "action-right" ]
        [ H.a [ HA.href <| UB.crossOrigin model.baseurl [ "tshistory", model.name ] [ ]
              , HA.target "_blank"
              ]
              [ H.text "browse history ⧉" ]
        , H.a [ HA.href <| UB.crossOrigin model.baseurl [ "tseditor" ]
                    [ UB.string "name" model.name ]
              , HA.target "_blank"
              ]
            [ H.text editorlabel ]
        , case model.seriestype of
              Formula ->
                  H.a [ HA.href <| UB.crossOrigin model.baseurl [ "tsformula" ]
                            [ UB.string "name" model.name ]
                      , HA.target "_blank"
                      ]
                      [ H.text "edit formula" ]
              Primary ->
                  H.span [ ] [ ]
        , if model.source == "local"
          then I.viewdeletion model "series" deleteevents
          else H.span [] []
        , if model.source == "local"
          then I.viewrenameaction model "series" renameevents
          else H.span [] []
        ]
    ]


viewtitle model =
    let
        ( tzaware, tzbadge, tztitle ) =
            if (M.dget "tzaware" model.meta) == "true"
            then ( "tzaware", "badge-success", "This series is time zone aware." )
            else ( "tznaive", "badge-warning", "This series is not associated with a time zone." )

        valuetype =
            M.dget "value_type" model.meta

        supervision =
            M.dget "supervision_status" model.meta
    in
    H.p
        [ ]
        [ H.i
              [ HA.class model.clipboardclass
              , HE.onClick CopyNameToClipboard
              ] [ ]
        , H.span
            [ HA.class "badges-spacing" ]
            [ H.span
                  [ HA.class "font-italic h4" ]
                  [ H.text <| " " ++ model.name ++ " " ]
            , H.span
                [ HA.class "badge h4"
                , HA.class tzbadge
                , HA.title tztitle
                ]
                [ H.text tzaware ]
            , H.span
                [ HA.class "badge badge-info h4"
                , HA.title "Supervision status of the series."
                ]
                [ H.text supervision ]
            , H.span
                [ HA.class "badge badge-primary h4"
                , HA.title "Type of the series values."
                ]
                [ H.text valuetype ]
            , H.span
                [ HA.class "badge badge-secondary h4"
                , HA.title "Name of the series source."
                ]
                [ H.text model.source ]
            ]
        ]


strtab tablelayout =
    case tablelayout of
        Plot -> "Plot"
        UserMetadata -> "Metadata"
        Logs -> "Logs"
        FormulaCache -> "Cache"


maketab model active tab =
    let
        tabname = strtab tab
    in
    H.li
        [ HA.class "nav-item" ]
        [ H.a
              ([ HE.onClick (Tab tab)
               , HA.class "nav-link"
               , HA.attribute "data-toggle" "tab"
               , HA.attribute "role" "tab"
               , HA.attribute "aria-selected" (if active then "true" else "false")
               , HA.id tabname
               ] ++ if active then [ HA.class "active" ] else []
              )
            [ H.div
                  []
                  [ H.text <| tabname ++ " " ]
            ]
        ]


header model tabs =
    H.ul [ HA.id "tabs"
         , HA.class "nav nav-tabs"
         , HA.attribute "role" "tablist"
         ]
        <| LS.toList
        <| LS.mapSelected
            { selected = maketab model True
            , rest = maketab model False
            }
            tabs


tabcontents items =
    H.span [ HA.style "margin" ".1rem" ]
        items


view : Model -> H.Html Msg
view model =
    let
        tablist =
            case model.seriestype of
                I.Primary ->
                    [ Plot, UserMetadata, Logs ]
                I.Formula ->
                    [ Plot, UserMetadata, FormulaCache ]

        tabs =
            tablist
                |> LS.fromList
                |> LS.select model.activetab

        head =
            header model tabs

    in
    H.div
        [ HA.style "margin" ".5em" ]
        [ H.span [ HA.class "action-container" ] <| viewactionwidgets model
        , viewtitle model
        , case model.activetab of
              Plot ->
                  if strseries model
                  then H.div [] [ head ]
                  else H.div []
                      [ head
                      , tabcontents
                            [ viewplot model
                            , I.viewformula model SwitchLevel
                            ]
                      ]

              UserMetadata ->
                  H.div [] [ head, tabcontents [ I.viewusermeta model metaevents False ] ]

              Logs ->
                  H.div []
                      [ head
                      , tabcontents
                            [ case model.seriestype of
                                  I.Primary -> I.viewlog model False
                                  I.Formula -> H.span [] []
                            ]
                      ]

              FormulaCache ->
                  H.div [] [ head, tabcontents [ viewcache model ] ]
        , I.viewerrors model
        ]


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input  Model Msg
main =
       let
           debouncerconfig =
               Debouncer.manual
                   |> settleWhenQuietFor (Just <| fromSeconds 0.015)
                   |> toDebouncer

           init input =
               let
                   model =
                       { baseurl = input.baseurl
                       , name = input.name
                       , source = ""
                       , activetab = Plot
                       -- metadata edition
                       , canwrite = False
                       , editing = False
                       -- all errors
                       , errors = [ ]
                       -- metadata
                       , meta = Dict.empty
                       , usermeta = Dict.empty
                       , seriestype = I.Primary
                       -- formula
                       , formula_depth = 0
                       , formula_maxdepth = 0
                       , formula = Dict.empty
                       -- cache
                       , has_cache = False
                       , view_nocache = False
                       , policy = Dict.empty
                       , deleting_cache = False
                       -- log
                       , log = [ ]
                       -- plot
                       , insertion_dates = Array.empty
                       , date_index = 0
                       , date_index_deb = debouncerconfig
                       -- user meta edittion
                       , metaitem = ("", "")
                       , editeditems = Dict.empty
                       -- deletion
                       , deleting = False
                       -- renaming
                       , renaming = False
                       , newname = Nothing
                       , clipboardclass = "bi bi-clipboard"
                       , horizon =
                            { offset = 0
                            , horizon = {key = Just defaultHorizon}
                            , inferredFreq = False
                            , mindate = ""
                            , maxdate = ""
                            , timeSeries = Dict.empty
                            , timeZone = "UTC"
                       }
                       , plotStatus = Loading
                    }
               in
               ( model
               , Cmd.batch
                   [ M.getsysmetadata input.baseurl input.name GotSysMeta "series"
                   , M.getusermetadata input.baseurl input.name GotUserMeta "series"
                   , getsource model model.name
                   , I.getwriteperms input.baseurl GetPermissions
                   , getcachepolicy model
                   ]
               )
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = \_ -> savedDataInCache SetDataInCache
               }
