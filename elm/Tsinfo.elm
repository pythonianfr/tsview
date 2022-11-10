module Tsinfo exposing (main)

import Array exposing (Array)
import Browser
import Debouncer.Messages as Debouncer exposing
    (Debouncer
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
import Json.Encode as E
import JsonTree as JT exposing (TaggedValue(..))
import Metadata as M
import Plotter exposing
    ( getplotdata
    , seriesdecoder
    , scatterplot
    , plotargs
    , Series
    )
import Url.Builder as UB
import Util as U


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : M.UserMetadata
    }


type alias Model =
    { baseurl : String
    , name : String
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    -- metadata, ventilated by std (system) and user
    , meta : M.StdMetadata
    , usermeta : M.UserMetadata
    -- formula
    , formula_expanded : Bool
    , formula : Maybe String
    , expanded_formula : Maybe String
    , formula_components : Maybe JT.Node
    , expanded_formula_components : Maybe JT.Node
    -- cache
    , has_cache : Bool
    , view_nocache : Bool
    , policy : M.StdMetadata
    , deleting_cache : Bool
    -- log
    , log : List Logentry
    -- plot
    , plotdata : Maybe Series
    , insertion_dates : Array String
    , mindate : String
    , maxdate : String
    , date_index : Int
    , date_index_deb : Debouncer Msg
    -- user meta edition
    , metaitem : (String, String)
    , editeditems : Dict String String
    }


type Msg
    = GotMeta (Result Http.Error String)
    | GetPermissions (Result Http.Error String)
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
    | CodeHighlight (Result Http.Error String)
    | Components (Result Http.Error String)
    | InsertionDates (Result Http.Error String)
    | ToggleExpansion
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


getplot model atidate =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getplotdata model.baseurl model.name
            (if atidate then idate else Nothing)
            GotPlotData
            (U.bool2int model.view_nocache)
            model.mindate
            model.maxdate


getformula : Model -> Cmd Msg
getformula model  =
    Http.get
        { expect = Http.expectString  GotFormula
        , url =
            UB.crossOrigin model.baseurl
                [ "api", "series", "formula" ]
                [ UB.string "name" model.name
                , UB.int "display" 1
                , UB.int "expanded" <| U.bool2int model.formula_expanded
                ]
        }


getlog : String -> String-> Cmd Msg
getlog urlprefix name  =
    Http.get
        { expect = Http.expectString GotLog
        , url = UB.crossOrigin urlprefix
              [ "api", "series", "log" ]
              [ UB.string "name" name
              , UB.int "limit" 10 ]
        }


pygmentyze model formula =
    Http.post
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsformula", "pygmentize" ]
              []
        , body = Http.stringBody "text/plain" formula
        , expect = Http.expectString CodeHighlight
        }


getcomponents model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "formula_components" ]
              [ UB.string "name" model.name
              , UB.int "expanded" <| U.bool2int model.formula_expanded
              ]
        , expect = Http.expectString Components
        }


gethascache model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-has-cache" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString HasCache
        }


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


getcachepolicy model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "cache", "series-policy" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString GotCachePolicy
        }


getidates model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "insertion_dates" ]
              [ UB.string "name" model.name
              , UB.int "nocache" <| U.bool2int model.view_nocache
              ]
        , expect = Http.expectString InsertionDates
        }


savemeta model =
    Http.request
        { method = "PUT"
        , body = Http.jsonBody <| E.object
                 [ ("name", E.string model.name )
                 , ("metadata" , E.string <| M.encodemeta model.usermeta )
                 ]
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "metadata" ] [ ]
        , expect = Http.expectString MetaSaved
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
                        next = getidates model
                        cmd = Cmd.batch [ getformula model, next ]
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
            case D.decodeString seriesdecoder rawdata of
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
                    ( model
                    , Cmd.batch [ pygmentyze model formula
                                , getcomponents model
                                , gethascache model
                                , getlog model.baseurl model.name
                                ]
                    )
                Err _ ->
                    -- there is no formula -> there must be logs !
                    ( model
                    , getlog model.baseurl model.name
                    )

        GotFormula (Err error) ->
            doerr "gotformula http" <| U.unwraperror error

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
                        , getidates newmodel
                        , getlog model.baseurl model.name
                        ]
            )

        CacheDeleted (Err error) ->
            doerr "cachedeleted http" <| U.unwraperror error

        ViewNocache ->
            let mod = { model | view_nocache = not model.view_nocache } in
            ( mod
            , Cmd.batch
                [ getidates mod
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

        -- code

        CodeHighlight (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    case model.formula_expanded of
                        True ->
                            U.nocmd { model | expanded_formula = Just formula }
                        False ->
                            U.nocmd { model | formula = Just formula }
                Err err ->
                    doerr "codehightlight decode" <| D.errorToString err

        CodeHighlight (Err error) ->
            doerr "codehighlight http" <| U.unwraperror error

        -- components

        Components (Ok rawcomponents) ->
            case JT.parseString rawcomponents of
                Ok components ->
                    case model.formula_expanded of
                        True ->
                            U.nocmd { model | expanded_formula_components = Just components }
                        False ->
                            U.nocmd { model | formula_components = Just components }
                Err err ->
                    doerr "components decode" <| D.errorToString err

        Components (Err error) ->
            doerr "components http" <| U.unwraperror error

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

        ToggleExpansion ->
            let
                state = model.formula_expanded
            in
                ( { model | formula_expanded = not state }
                , case model.expanded_formula of
                      Nothing ->
                          getformula { model | formula_expanded = not state }
                      Just _ ->
                          Cmd.none
                )

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
                Nothing -> ( model, Cmd.none )
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

        FvdatePickerChanged value ->
            let
                newmodel = { model | mindate = value }
            in
                ( newmodel
                , getplot newmodel True
                )

        TvdatePickerChanged value ->
            let
                newmodel = { model | maxdate = value }
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
            ( { model
                  | metaitem = ("", "")
                  , editeditems = edited
              }
            , Cmd.none
            )

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
            ( newmodel, savemeta newmodel )

        MetaSaved (Ok _) ->
            U.nocmd { model
                        | editing = False
                        , editeditems = Dict.empty
                        , metaitem = ("", "")
                    }

        MetaSaved (Err err) ->
            doerr "metasaved http" <| U.unwraperror err


-- views


metadicttostring d =
    let
        builditem ab =
            U.first ab ++ " → " ++ (M.metavaltostring <| U.snd ab)
    in
    String.join "," <| List.map builditem (Dict.toList d)


viewlogentry entry =
    tr [ ]
        [ th [ A.scope "row" ] [ text (String.fromInt entry.rev) ]
        , td [ ] [ text entry.author ]
        , td [ ] [ text entry.date ]
        , td [ ] [ text <| metadicttostring entry.meta ]
        ]


viewlog model showtitle =
    if List.length model.log > 0 then
        div [ ]
            [ if showtitle
              then h2 [ ] [ text "History Log" ]
              else span [] []
            , table [A.class "table table-striped table-hover table-sm"]
                [ thead [ ]
                      [ td [ A.scope "col" ] [ text "#" ]
                      , td [ A.scope "col" ] [ text "author" ]
                      , td [ A.scope "col" ] [ text "date" ]
                      , td [ A.scope "col" ] [ text "meta" ]
                      ]
                , tbody [ ] <| List.map viewlogentry (List.reverse model.log)
                ]
            ]
    else div [ ] [ ]


viewcachepolicy model =
    let
        names = [ "name", "look_before", "look_after", "revdate_rule", "schedule_rule" ]
        fixval name val =
            if name == "supervision_status" && val == ""
            then "formula"
            else val
        elt name =
            li [ ] [text <| name
                        ++ " → "
                        ++ (M.dget name model.policy)
                   ]
    in
    div [ ]
    [ h2 [ ] [ text "Policy" ]
    , ul [ A.class "highlight" ] <| List.map elt names
    ]


viewtogglecached model =
    div
    [ A.class "custom-control custom-switch"
    , A.title <| if model.view_nocache
                 then "view cached"
                 else "view uncached"
    ]
    [ input
          [ A.attribute "type" "checkbox"
          , A.class "custom-control-input"
          , A.id "view-uncached"
          , A.checked <| not model.view_nocache
          , onClick ViewNocache
          ] [ ]
    , label
          [ A.class "custom-control-label"
          , A.for "view-uncached"
          ]
          [ text <| if model.view_nocache
                    then "view uncached"
                    else "view cached"
          ]
    ]


viewcache model =
    let
        cachecontrol =
            span [ ]
                [ if List.length model.log > 0
                  then viewlog model False
                  else span [ ] [ ]
                , if Dict.isEmpty model.policy
                  then span [ ] [ ]
                  else viewcachepolicy model
                , viewtogglecached model
                ]

        deleteaction =
            if model.has_cache then
                if model.deleting_cache then
                    span [ ]
                        [ button [ A.class "btn btn-warning"
                                 , A.attribute "type" "button"
                                 , onClick CacheCancelDeletion ]
                              [ text "cancel" ]
                        , span [ ] [ text " " ]
                        , button [ A.class "btn btn-danger"
                                 , A.attribute "type" "button"
                                 , onClick CacheConfirmDeletion ]
                              [ text "confirm" ]
                        ]
                else
                    button [ A.class "btn btn-danger"
                           , A.attribute "type" "button"
                           , A.title "This is an irreversible operation."
                           , onClick DeleteCache ]
                    [ text "delete" ]
            else
                span [ ] [ ]

    in
    if I.supervision model == "formula" then
        div [ ]
            [ h2 [ ] [ text "Cache"
                    , span [ ] [ text " " ]
                    , deleteaction
                    ]
            , cachecontrol
            ]
    else
        div [ ] [ ]


viewidatepicker model =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date
    in div
        [ ]
        [ label [ A.for "idate-picker" ] [ text "revision date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "datetime-local"
                , A.id "idate-picker"
                , A.name "idate-picker"
                , A.value currdate
                , onInput IdatePickerChanged
                ] [ ]
        , span [ ] [ text " " ]
        , label [ A.for "fvd-picker" ] [ text "from value date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "date"
                , A.id "fvd-picker"
                , A.name "fvd-picker"
                , A.value model.mindate
                , onInput FvdatePickerChanged
                ] [ ]
        , span [ ] [ text " " ]
        , label [ A.for "tvd-picker" ] [ text "to value date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "date"
                , A.id "tvd-picker"
                , A.name "tvd-picker"
                , A.value model.maxdate
                , onInput TvdatePickerChanged
                ] [ ]
        ]


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


viewplot model =
    let
        plotdata = case model.plotdata of
                       Nothing -> Dict.empty
                       Just data -> data

        plot = scatterplot model.name
               (Dict.keys plotdata)
               (Dict.values plotdata)
               "lines"
        args = plotargs "plot" [plot]
    in
    div [ ]
        [ h2 [ ] [ text "Plot" ]
        , viewidatepicker model
        , viewdatesrange model
        , div [ A.id "plot" ] [ ]
        -- the "plot-figure" node is pre-built in the template side
        -- (html component)
        , node "plot-figure" [ A.attribute "args" args ] [ ]
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


view : Model -> Html Msg
view model =
    div [ A.style "margin" ".5em" ]
        [ h1 [ ]
              [ text "Series "
              , span
                    [ A.class "font-italic" ]
                    [ text model.name ]
              ]
        , I.viewseealso model
        , I.viewmeta model
        , I.viewusermeta model metaevents
        , I.viewformula model ToggleExpansion
        , case model.formula of
              Nothing -> viewlog model True
              Just _ -> span [] []
        , I.viewcomponents model
        , viewcache model
        , viewplot model
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
                   model = Model
                           input.baseurl
                           input.name
                           -- metadata edition
                           False
                           False
                           -- all errors
                           [ ]
                           -- metadata
                           Dict.empty
                           Dict.empty
                           -- formula
                           False
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           -- cache
                           False
                           False
                           Dict.empty
                           False
                           -- log
                           [ ]
                           -- plot
                           Nothing
                           Array.empty
                           ""
                           ""
                           0
                           debouncerconfig
                           -- user meta edittion
                           ("", "")
                           Dict.empty
               in
               ( model
               , Cmd.batch
                   [ M.getmetadata input.baseurl input.name GotMeta "series"
                   , getplot model False
                   , I.getwriteperms input.baseurl GetPermissions
                   , getcachepolicy model
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
