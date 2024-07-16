port module Groupinfo exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation exposing (load)
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Info as I
import Json.Decode as D
import List.Selection as LS
import Metadata as M
import Task as T
import NavTabs as Nav exposing
    ( header
    , tabcontents
    , strseries
    , Tabs(..)
    , viewdatespicker
     , DeleteEvents
     , MetaEvents
    )
import Plotter exposing
    ( defaultLayoutOptions
    , defaultoptions
    , getgroupplotdata
    , groupdecoder
    , scatterplot
    , plotargs
    , Group
    )
import Process as P
import Task as T
import Url.Builder as UB
import Util as U


port copyToClipboard : String -> Cmd msg


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : M.UserMetadata
    }


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
    , source : String
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    -- metadata, ventilated by std (system) and user
    , meta : M.StdMetadata
    , usermeta : M.UserMetadata
    -- formula
    , formula_depth : Int
    , formula_maxdepth : Int
    , formula : Dict Int String
    , bindings : Maybe Bindings
    -- cache (none yet but minimal data model suppport for genericity)
    , view_nocache : Bool
    -- log
    , log : List Logentry
    , logsNumber : Maybe Int
    -- plot
    , plotdata : Maybe Group
    , insertion_dates : Array String
    , mindate : String
    , maxdate : String
    , date_index : Int
    , date_index_deb : Debouncer Msg
    -- user meta edition
    , metaitem : (String, String)
    , editeditems : Dict String String
    -- deletion
    , deleting : Bool
    , clipboardclass : String
    , activetab : Tabs
    }


type Msg
    = GotMeta (Result Http.Error String)
    | GetPermissions (Result Http.Error String)
    | GotPlotData (Result Http.Error String)
    -- dates
    | ChangedIdate String
    | DebounceChangedIdate (Debouncer.Msg Msg)
    | IdatePickerChanged String
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
    | CopyNameToClipboard
    | ResetClipboardClass
    | Tab Tabs
    | LogsNumber String
    | SeeLogs


getplot : Model -> Bool -> Cmd Msg
getplot model atidate =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getgroupplotdata model.baseurl model.name
            (if atidate then idate else Nothing)
            GotPlotData
            model.mindate
            model.maxdate



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
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GotMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    let
                        (stdmeta, usermeta) =
                            Dict.partition (\k v -> (List.member k M.metanames)) allmeta
                        newmodel =
                            { model
                                | meta = stdmeta
                                , usermeta = usermeta
                            }
                        next = I.getidates model "group" InsertionDates
                        cmd =
                            Cmd.batch
                                [ I.getformula model model.name 0 "group" GotFormula, next ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotMeta (Err err) ->
            doerr "gotmeta http"  <| U.unwraperror err

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
                        dates = Dict.keys val
                        minappdate =
                            case dates of
                                head::_ -> U.cleanupdate head
                                []  -> ""
                        maxappdate = U.cleanupdate <| Maybe.withDefault "" <| List.maximum dates
                        newmodel =
                            case model.plotdata of
                                Nothing ->
                                    { model
                                        | plotdata = Just val
                                        , mindate = U.dateof minappdate
                                        , maxdate = U.dateof maxappdate
                                    }
                                Just data -> { model | plotdata = Just val }
                    in
                    U.nocmd newmodel
                Err err ->
                    doerr "gotplotdata decode" <| D.errorToString err

        GotPlotData (Err err) ->
            doerr "gotplotdata error" <| U.unwraperror err

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

        -- insertiond ates

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
            in ( newmodel
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
            , I.savemeta newmodel "group" MetaSaved
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
            , I.delete model "group" Deleted
            )

        Deleted (Ok _) ->
            ( model
            , load <| UB.crossOrigin model.baseurl [ "tssearch" ] [ ]
            )

        Deleted (Err err) ->
            doerr "deletion failed" <| U.unwraperror err

        Tab tab ->
            U.nocmd { model | activetab = tab }

        CopyNameToClipboard ->
            ( { model | clipboardclass = "bi bi-check2" }
            , Cmd.batch
                [ copyToClipboard model.name
                , T.perform (always (ResetClipboardClass)) (P.sleep 1000)
                ]
            )

        ResetClipboardClass ->
            U.nocmd { model | clipboardclass = "bi bi-clipboard" }

        LogsNumber strLogsNumber ->
            U.nocmd { model | logsNumber = String.toInt strLogsNumber }

        SeeLogs ->
            U.nocmd model


-- views


viewdatesrange : Model -> Html Msg
viewdatesrange model =
    let
        numidates = Array.length model.insertion_dates
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> date
    in
    if numidates < 2
    then div [ ] [ ]
    else
        Html.map (provideInput >> DebounceChangedIdate) <|
            div [ ]
            [ input
                  [ A.attribute "type" "range"
                  , A.min "0"
                  , A.max (String.fromInt (numidates - 1))
                  , A.value (String.fromInt model.date_index)
                  , A.class "form-control-range"
                  , A.title currdate
                  , onInput ChangedIdate
                  ] [ ]
            ]


viewplot : Model -> Html Msg
viewplot model =
    let
        groupdata =
            case model.plotdata of
                Nothing -> Dict.empty
                Just data -> data

        plot (name, plotdata) =
            scatterplot
                name
                (Dict.keys plotdata)
                (Dict.values plotdata)
                "lines"
                defaultoptions

        plots = List.map plot <| Dict.toList groupdata

        args =
            plotargs "plot" plots defaultLayoutOptions
    in
    div [ ]
        [ viewdatespicker
            model.date_index
            model.insertion_dates
            IdatePickerChanged
        , viewdatesrange model
        , div [ A.id "plot" ] [ ]
        -- the "plot-figure" node is pre-built in the template side
        -- (html component)
        , node "plot-figure" [ A.attribute "args" args ] [ ]
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
                    [ Plot, UserMetadata, Logs ]
                Just _ ->
                    [ Plot, UserMetadata, FormulaCache ]

        tabs =
            tablist
                |> LS.fromList
                |> LS.select model.activetab

        head =
            Nav.header Tab tabs

        deleteEvents = DeleteEvents
                ConfirmDeletion
                CancelDeletion
                AskDeletion

        metaEvents = MetaEvents
            MetaEditAsked
            MetaEditCancel
            EditedValue
            MetaItemToDelete
            NewKey
            NewValue
            SaveMeta
            AddMetaItem

    in
        div
            [ ]
            [ div
                [ A.class "main-content" ]
                [ div
                    [ ]
                    [ span
                        [ A.class "groupinfo action-container" ]
                        [ div
                            [ A.class "page-title" ]
                            [ text "Group Info" ]
                        , I.viewdeletion model deleteEvents ]
                    , I.viewtitle model Nothing CopyNameToClipboard
                    , viewbindings model
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

                        UserMetadata ->
                            div [] [ head, tabcontents [ I.viewusermeta model metaEvents False ] ]

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
                            div [] []
                    , I.viewerrors model
                    ]
                ]
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
                       , source = "local"
                       -- metadata edition
                       , canwrite = False
                       , editing = False
                       -- all errors
                       , errors = [ ]
                       -- metadata
                       , meta = Dict.empty
                       , usermeta = Dict.empty
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
                       , mindate = ""
                       , maxdate = ""
                       , date_index = 0
                       , date_index_deb = debouncerconfig
                       -- user meta edittion
                       , metaitem = ("", "")
                       , editeditems = Dict.empty
                       -- deletion
                       , deleting = False
                       , activetab = Plot
                       , clipboardclass = "bi bi-clipboard"
                       }
               in
               ( model
               , Cmd.batch
                   [ M.getsysmetadata input.baseurl input.name GotMeta "group"
                   , getplot model False
                   , I.getwriteperms input.baseurl GetPermissions
                   , getbindings model
                   ]
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
