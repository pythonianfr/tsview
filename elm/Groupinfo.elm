port module Groupinfo exposing (main)

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
    , updateZoom
    )
import Horizon as HorizonModule
import Html exposing (..)
import Html.Attributes as A
import Http
import Info as I
import Info exposing ( Direction(..))
import Json.Decode as D
import List.Selection as LS
import Metadata as M
import Task as T
import NavTabs as Nav exposing
    ( tabcontents
    , Tabs(..)
     , DeleteEvents
     , MetaEvents
    )
import Plotter exposing
    ( defaultDateAxis
    , defaultLayoutOptions
    , defaultTraceOptions
    , defaultValueAxis
    , getgroupplotdata
    , groupdecoder
    , Group
    )
import Process as P
import Task as T
import Url.Builder as UB
import Util as U

port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg
port copyToClipboard : String -> Cmd msg
port panActive : (Bool -> msg) -> Sub msg

type alias Binding =
    { family : String
    , group : String
    , series : String
    }


type alias Bindings =
    { name : String
    , bindings : List Binding
    }


type alias Model =
    { baseurl : String
    , name : String
    , tzaware : Bool
    , source : String
    , activetab : Tabs
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    , doesnotexist: Bool
    -- metadata, ventilated by std (system) and user
    , meta : M.Metadata
    , grouptype : I.GroupType
    , usermeta : M.Metadata
    , metahistory : List I.OldMetadata
    -- formula
    , formula_depth : Int
    , formula_maxdepth : Int
    , formula : Dict Int String
    , bindings : Maybe Bindings
    -- cache (none yet but minimal data model support for genericity)
    , view_nocache : Bool
    -- log
    , log : List I.Logentry
    , logsNumber : Maybe Int
    -- plot
    , plotdata : Maybe Group
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
    }


type alias RenameEvents =
    { confirmrename : Msg
    , editnewname : String -> Msg
    , cancelrename : Msg
    , askrename : Msg
    }


setStatus: Model -> PlotStatus -> Model
setStatus model status =
    let horizon = model.horizon
    in
        { model | horizon = { horizon | plotStatus = status }}


type Msg
    = GotSysMeta (Result Http.Error String)
    | GotUserMeta (Result Http.Error String)
    | GotSource (Result Http.Error String)
    | GotMetaHistory (Result Http.Error String)
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
    | IterDate Bool Direction
    -- formula
    | GotFormula (Result Http.Error String)
    | InsertionDates (Result Http.Error String)
    | GotBindings (Result Http.Error String)
    | SwitchLevel String
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
    | FromZoom ZoomFromPlotly
    | NewDragMode Bool
    -- logs
    | LogsNumber String
    | SeeLogs


convertMsg : HorizonModule.Msg -> Msg
convertMsg msg =
    Horizon msg


getsource : String -> String -> Cmd Msg
getsource baseurl name =
    Http.get
        { expect = Http.expectString GotSource
        , url = UB.crossOrigin baseurl
              [ "api", "group", "source" ]
              [ UB.string "name" name ]
        }


getplot : Model -> Cmd Msg
getplot model =
    let
        idate =
            Array.get model.date_index model.insertion_dates
        ( start, end ) = getFetchBounds model.horizon
    in
        getgroupplotdata
            { baseurl = model.baseurl
            , name = model.name
            , idate = idate
            , callback = GotPlotData
            , fromdate = start
            , todate = end
            , horizon = Nothing
            , tzone = model.horizon.timeZone
            , tzware = model.tzaware
            , keepnans = False
            , apipoint = "state"
            , tracker = Nothing
            }

bindingdecoder : D.Decoder Binding
bindingdecoder =
    (D.map3 Binding
         (D.field "family" D.string)
         (D.field "group" D.string)
         (D.field "series" D.string))


bindingsdecoder : D.Decoder Bindings
bindingsdecoder =
    D.map2 Bindings
        (D.field "name" D.string)
        (D.field "bindings"
             (D.list bindingdecoder))


getbindings : Model -> Cmd Msg
getbindings model =
    Http.get
        { expect = Http.expectString GotBindings
        , url =
            UB.crossOrigin model.baseurl
                [ "api", "group", "boundformula" ]
                [ UB.string "name" model.name ]
        }


updatedchangedidatebouncer =
    { mapMsg = DebounceChangedIdate
    , getDebouncer = .date_index_deb
    , setDebouncer = \deb model -> { model | date_index_deb = deb }
    }


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
                        isbindings = Dict.member "bound" allmeta
                        seriestype =
                            if isformula
                            then I.GroupFormula
                            else if isbindings
                                    then I.GroupBound
                                    else I.GroupPrimary
                        (stdmeta, usermeta) =
                            Dict.partition (\k v -> (List.member k M.metanames)) allmeta
                        newmodel =
                            { model
                                | meta = stdmeta
                                , tzaware = (M.dget "tzaware" stdmeta) == "true"
                                , grouptype = seriestype
                            }
                        cmd =
                            Cmd.batch <| [ ]
                                ++ if isformula
                                   then [ I.getformula
                                            model model.name 0 "group" GotFormula
                                        ]
                                   else if isbindings
                                   then [ getbindings model ]
                                   else [ I.getlog model.baseurl model.name model.logsNumber "group" GotLog ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotSysMeta (Err err) ->
            doerr "gotmeta http"  <| U.unwraperror err

        GotUserMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    U.nocmd { model | usermeta = allmeta }
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotUserMeta (Err err) ->
            doerr "gotusermeta http"  <| U.unwraperror err

        GotMetaHistory (Ok rawhist) ->
            case D.decodeString I.oldmetasdecoder rawhist of
                Ok hist ->
                    U.nocmd { model | metahistory = hist }
                Err err ->
                    doerr "gotmetahistory decode" <| D.errorToString err

        GotMetaHistory (Err err) ->
            doerr "gotmetahistory http"  <| U.unwraperror err

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
            case D.decodeString groupdecoder rawdata of
                Ok val ->
                    let
                        val_ts = (Maybe.withDefault (Dict.fromList [] )
                                       (Dict.get (Maybe.withDefault "" (List.head (Dict.keys val))) val))
                        newmodel =
                            { model
                                | plotdata = Just val
                                , horizon = updateHorizonFromData
                                                model.horizon
                                                (Dict.keys val_ts)
                            }
                    in
                    U.nocmd newmodel
                Err err ->
                    doerr "gotplotdata decode" <| D.errorToString err

        GotPlotData (Err err) ->
            case err of
                Http.BadStatus code ->
                    if code == 404
                    then U.nocmd { model | doesnotexist = True }
                    else doerr "gotplotdata error" <| U.unwraperror err
                _ -> doerr "gotplotdata error" <| U.unwraperror err

        GotFormula (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    U.nocmd { model | formula =
                                  Dict.insert model.formula_depth formula model.formula
                            }
                Err _ ->
                    -- there is no formula -> there might be logs !
                    -- but right now we don't have them anyway
                    U.nocmd model

        GotFormula (Err error) ->
            doerr "gotformula http" <| U.unwraperror error

        GotBindings (Ok rawbindings) ->
            case D.decodeString bindingsdecoder rawbindings of
                Ok bindings ->
                    let
                        newmodel =
                            { model | bindings = Just bindings }
                    in
                    ( newmodel
                    , I.getformula newmodel bindings.name 0 "series" GotFormula
                    )
                Err _ ->
                    U.nocmd model

        GotBindings (Err error) ->
            U.nocmd model

        SwitchLevel level ->
            let
                depth = Maybe.withDefault 0 <| String.toInt level
            in
            U.nocmd { model | formula_depth = depth }

        -- log

        GotLog (Ok rawlog) ->
            case D.decodeString I.logdecoder rawlog of
                Ok log ->
                    U.nocmd { model | log = log }
                Err err ->
                    doerr "gotlog decode" <| D.errorToString err

        GotLog (Err error) ->
            doerr "gotlog http" <| U.unwraperror error

        -- insertion dates

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
                    ( setStatus newmodel Loading
                    , getplot newmodel
                    )

        IdatePickerChanged value ->
            let
                comparedates d1 d2 =
                    d1 > d2
                newarray =  Array.filter (comparedates value) <|
                            Array.map U.cleanupdate model.insertion_dates
                newindex = max 0 <| Array.length newarray - 1
                newmodel = { model | date_index = newindex }
            in ( setStatus newmodel Loading
               , getplot newmodel
               )

        IterDate _ direction ->
            let newM = case direction of
                        Prev -> { model |
                            date_index = ( model.date_index - 1 )}
                        Next -> { model |
                            date_index = ( model.date_index + 1 )}
            in
                ( setStatus newM Loading
                , getplot newM )

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
            , I.savemeta newmodel "group" MetaSaved
            )

        MetaSaved (Ok _) ->
            ( { model
                  | editing = False
                  , editeditems = Dict.empty
                  , metaitem = ("", "")
              }
            , I.getoldmetadata model.baseurl model.name GotMetaHistory "group"
            )

        MetaSaved (Err err) ->
            doerr "metasaved http" <| U.unwraperror err

        -- deletion

        AskDeletion ->
            U.nocmd { model | deleting = True }

        CancelDeletion ->
            U.nocmd { model | deleting = False }

        ConfirmDeletion ->
            ( model
            , I.delete model "group" Deleted
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
                            I.rename model newname "group" Renamed
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
            , load <| UB.crossOrigin model.baseurl [ "groupinfo" ] [ UB.string "name" name ]
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
            let ( newhorizonmodel, commands ) =
                    updateHorizon
                    hmsg
                    convertMsg
                    model.horizon
            in
            let newmodel = { model | horizon =  newhorizonmodel }
                default = ( newmodel, commands )
            in
            case hmsg of
                HorizonModule.Fetch _ ->
                    ( { newmodel | insertion_dates = Array.empty }
                     , Cmd.batch ([ commands
                                  , getplot newmodel
                                  , getIdates
                                        newmodel.horizon
                                        "group"
                                        InsertionDates
                                        model.name
                                  ])
                    )
                _ -> default

        FromZoom zoom ->
            let
                horizonmodel = model.horizon
                newZoom = updateZoom
                                model.horizon
                                ( extractZoomDates zoom )
            in
                U.nocmd { model | horizon =
                            { horizonmodel | zoomBounds = newZoom.x
                                           , zoomY = newZoom.y
                            }}

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }

        LogsNumber strLogsNumber ->
            U.nocmd { model | logsNumber = String.toInt strLogsNumber }

        SeeLogs ->
            ( model
            , I.getlog model.baseurl model.name model.logsNumber "group" GotLog
            )


-- views

viewplot : Model -> Html Msg
viewplot model =
    let
        groupdata =
            case model.plotdata of
                Nothing -> Dict.empty
                Just data -> data

        defaultLayout =
            { defaultLayoutOptions |
                xaxis = { defaultDateAxis
                            | range = extractDates
                                        <| getFromToDates
                                            model.horizon
                        }
                , yaxis = { defaultValueAxis |
                                range = extractValues
                                            model.horizon.zoomY
                          }
                , dragMode = Just ( if model.panActive
                                    then "pan"
                                    else "zoom" )}
    in
        div []
            [ I.viewDatesRange
                model.insertion_dates
                model.date_index
                DebounceChangedIdate
                ChangedIdate
            , I.viewWidgetIdates
                False
                model.insertion_dates
                model.horizon.timeZone
                model.date_index
                IterDate
            , I.viewgraph
                groupdata
                defaultLayout
                defaultTraceOptions
                model.horizon.inferredFreq
            ]


viewbindings : Model -> Html Msg
viewbindings model =
    let
        viewitem accessor item =
            td [ ]
                [ a
                  [ A.href <| UB.crossOrigin model.baseurl [ accessor ] [ UB.string "name" item ]
                  , A.target "_blank"
                  ]
                  [ text item ]
                ]

        viewbinding binding =
            tr [ ]
                [ th [ A.scope "row" ] [ text binding.family ]
                , viewitem "tsinfo" binding.series
                , viewitem "groupinfo" binding.group
                ]
    in
    case model.bindings of
        Nothing -> span [ ] [ ]
        Just bindings ->
            div
            [ ] <|
            [ div
              [ ]
              [ span [ ] [ text "This group is defined by this formula: " ]
              , a [ A.href <| UB.crossOrigin
                        model.baseurl
                        [ "tsinfo" ]
                        [ UB.string "name" bindings.name ]
                  , A.target "_blank"
                  ] [ text bindings.name ]
              , p [ A.title "In a given family, group members are matched." ]
                  [ text "In this formula the following series are replaced by groups." ]
              ]
            , table
                  [ A.class "table table-responsive table-hover" ]
                  [ thead [ A.class "thead-light" ]
                        [ tr [ ]
                              [ th [ A.scope "col" ] [ text "family" ]
                              , th [ A.scope "col" ] [ text "series" ]
                              , th [ A.scope "col" ] [ text "group" ]
                              ]
                        ]
                  , tbody
                        [ ]
                        (List.map viewbinding bindings.bindings)
                  ]
            ]


view : Model -> Html Msg
view model =
    let
        tablist =
            case Dict.get model.formula_depth model.formula of
                Nothing ->
                    [ Plot, Metadata, Logs ]
                Just _ ->
                    [ Plot, Metadata, FormulaCache ]

        tabs =
            tablist
                |> LS.fromList
                |> LS.select model.activetab

        head =
            Nav.header Tab tabs

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

    in
    div [ ]
        [ div
          [ A.class "main-content" ]
          [ div
            [ ]
            ( [ span
                  [ A.class "groupinfo action-container" ]
                  <| (I.viewactionwidgets
                            model
                            (I.GroupType model.grouptype)
                            convertMsg
                            Nothing
                            True
                            "Group Info"
                            ( getFromToDates model.horizon )
                        ) ++
                  [ I.viewdeletion model deleteEvents
                  , I.viewrenameaction model renameEvents
                  ]
            , I.viewtitle model model.clipboardclass CopyNameToClipboard ] ++
                if model.doesnotexist
                then
                [ I.msgdoesnotexist "Group"]
                else
            [ viewbindings model
            , case model.activetab of
                  Plot ->
                      if Nav.strseries model.meta
                      then div [] [ head ]
                      else
                          div []
                              [ head
                              , tabcontents
                                    [ viewplot model
                                    , I.viewformula model SwitchLevel
                                    ]
                              ]

                  Metadata ->
                      div [] [ head, tabcontents [ I.viewusermeta model metaEvents False ] ]

                  Folder ->
                      div [] [ head, tabcontents [ ] ]

                  Logs ->
                      div []
                          [ head
                          , tabcontents
                                [ case Dict.get model.formula_depth model.formula of
                                      Nothing -> I.viewlog model False LogsNumber SeeLogs
                                      Just _ -> span [] []
                                ]
                          ]

                  FormulaCache ->
                      div [] [ head ]

                  FormulaHistory ->
                      div [] [ head ]

                  Dependents ->
                      div [] [ head ]

            , I.viewerrors model
            ]
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


main : Program Input Model Msg
main =
       let
           debouncerconfig =
               Debouncer.manual
                   |> settleWhenQuietFor (Just <| fromSeconds 0.015)
                   |> toDebouncer

           init input =
               let
                   horizon = initHorizon
                                input.baseurl
                                input.min
                                input.max
                                input.debug
                                Loading
                   model =
                       { baseurl = input.baseurl
                       , name = input.name
                       , tzaware = True
                       , source = "local"
                       -- metadata edition
                       , canwrite = False
                       , editing = False
                       -- all errors
                       , errors = [ ]
                       , doesnotexist= False
                       -- metadata
                       , meta = Dict.empty
                       , grouptype = I.GroupPrimary
                       , usermeta = Dict.empty
                       , metahistory = []
                       -- formula
                       , formula_depth = 0
                       , formula_maxdepth = 0
                       , formula = Dict.empty
                       , bindings = Nothing
                       -- cache
                       , view_nocache = False
                       -- log
                       , log = [ ]
                       , logsNumber  = Just 10
                       -- plot
                       , plotdata = Nothing
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
                       , activetab = Plot
                       , clipboardclass = "bi bi-clipboard"
                       , horizon = { horizon | hasCache = False
                                             , hasInferredFreq = False }
                       }
               in
               ( model
               , Cmd.batch
                   [ M.getsysmetadata input.baseurl input.name GotSysMeta "group"
                   , M.getusermetadata input.baseurl input.name GotUserMeta "group"
                   , I.getoldmetadata input.baseurl input.name GotMetaHistory "group"
                   , I.getwriteperms input.baseurl GetPermissions
                   , getsource input.baseurl input.name
                   ]
               )
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions =
                    \_ -> Sub.batch
                      [ zoomPlot FromZoom
                      , panActive NewDragMode
                      , loadFromLocalStorage
                            (\ s -> convertMsg (HorizonModule.FromLocalStorage s))
                      ]
               }
