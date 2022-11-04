module Cache exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra as LE
import Set exposing (Set)
import Url.Builder as UB


nocmd model = ( model, Cmd.none )


unwraperror : Http.Error -> String
unwraperror resp =
    case resp of
        Http.BadUrl x -> "bad url: " ++ x
        Http.Timeout -> "the query timed out"
        Http.NetworkError -> "there was a network error"
        Http.BadStatus val -> "we got a bad status answer: " ++ String.fromInt val
        Http.BadBody body -> "we got a bad body: " ++ body


type alias Policy =
    { name : String
    , ready : Bool
    , initial_revdate : String
    , look_before : String
    , look_after : String
    , revdate_rule : String
    , schedule_rule : String
    , active : Bool
    }


type alias PolicyError =
    { initial_revdate : Maybe String
    , look_before : Maybe String
    , look_after : Maybe String
    , revdate_rule : Maybe String
    , schedule_rule : Maybe String
    }


type alias Model =
    { baseurl : String
    , policies : List Policy
    , deleting : Maybe String
    , adding : Maybe Policy
    , editing : Maybe Policy
    , editerror : Maybe PolicyError
    , editerrormsg : String
    , linking : Maybe Policy
    , cachedseries : List String
    , cachedseriesquery : String
    , cachedseriesformulaquery : String
    , freeseries : List String
    , freeseriesquery : String
    , freeseriesformulaquery : String
    , addtocache : Set String
    , removefromcache : Set String
    }


policydecoder =
    D.map8 Policy
        (D.field "name" D.string)
        (D.field "ready" D.bool)
        (D.field "initial_revdate" D.string)
        (D.field "look_before" D.string)
        (D.field "look_after" D.string)
        (D.field "revdate_rule" D.string)
        (D.field "schedule_rule" D.string)
        (D.field "active" D.bool)


policy_error_decoder =
    D.map5 PolicyError
        (D.maybe (D.field "initial_revdate" D.string))
        (D.maybe (D.field "look_before" D.string))
        (D.maybe (D.field "look_after" D.string))
        (D.maybe (D.field "revdate_rule" D.string))
        (D.maybe (D.field "schedule_rule" D.string))


policiesdecoder =
    D.list policydecoder


getpolicies model =
    Http.get
    { url = UB.crossOrigin model.baseurl
          [ "policies" ] [ ]
    , expect = Http.expectJson GotPolicies policiesdecoder
    }


seriesdecoder =
    D.list D.string


getcachedseries model policy =
    Http.get
    { url = UB.crossOrigin model.baseurl
          [ "policy-series/" ++ policy.name ] [ ]
    , expect = Http.expectJson GotCachedSeries seriesdecoder
    }


getfreeseries model policy =
    Http.get
    { url = UB.crossOrigin model.baseurl
          [ "cacheable-formulas" ] [ ]
    , expect = Http.expectJson GotFreeSeries seriesdecoder
    }


setcache model policyname seriesname =
    let payload_encoder =
            [ ("policyname" , E.string policyname)
            , ("seriesname", E.string seriesname)
            ]
    in Http.request
    { url = UB.crossOrigin model.baseurl [ "set-series-policy" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object payload_encoder
    , expect = Http.expectString CacheWasSet
    , timeout = Nothing
    , tracker = Nothing
    }


unsetcache model name =
    let payload_encoder =
            [ ("name" , E.string name) ]
    in Http.request
    { url = UB.crossOrigin model.baseurl [ "unset-series-policy" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object payload_encoder
    , expect = Http.expectString CacheWasUnset
    , timeout = Nothing
    , tracker = Nothing
    }


validatepolicy model policy =
    let policy_encoder =
            [ ("initial_revdate", E.string policy.initial_revdate)
            , ("look_before", E.string policy.look_before)
            , ("look_after", E.string policy.look_after)
            , ("revdate_rule", E.string policy.revdate_rule)
            , ("schedule_rule", E.string policy.schedule_rule)
            ]
    in Http.request
    { url = UB.crossOrigin model.baseurl [ "validate-policy" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_encoder
    , expect = Http.expectString ValidatedPolicy
    , timeout = Nothing
    , tracker = Nothing
    }


sendpolicy model policy =
    let policy_encoder =
            [ ("name" , E.string policy.name)
            , ("initial_revdate", E.string policy.initial_revdate)
            , ("look_before", E.string policy.look_before)
            , ("look_after", E.string policy.look_after)
            , ("revdate_rule", E.string policy.revdate_rule)
            , ("schedule_rule", E.string policy.schedule_rule)
            ]
    in Http.request
    { url = UB.crossOrigin model.baseurl [ "create-policy" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_encoder
    , expect = Http.expectString CreatedPolicy
    , timeout = Nothing
    , tracker = Nothing
    }


updatepolicy model policy =
    let policy_encoder =
            [ ("name" , E.string policy.name)
            , ("initial_revdate", E.string policy.initial_revdate)
            , ("look_before", E.string policy.look_before)
            , ("look_after", E.string policy.look_after)
            , ("revdate_rule", E.string policy.revdate_rule)
            , ("schedule_rule", E.string policy.schedule_rule)
            ]
    in Http.request
    { url = UB.crossOrigin model.baseurl [ "edit-policy" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_encoder
    , expect = Http.expectString UpdatedPolicy
    , timeout = Nothing
    , tracker = Nothing
    }


deletepolicy model name =
    Http.request
    { url = UB.crossOrigin model.baseurl [ "delete-policy", name ] [ ]
    , method = "DELETE"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectString DeletedPolicy
    , timeout = Nothing
    , tracker = Nothing
    }


activate model policy =
    let policy_name_encoder =
            [ ("name" , E.string policy.name) ]
    in
    Http.request
    { url = UB.crossOrigin model.baseurl
          [ "schedule-policy" ]
          [ UB.string "name" policy.name ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_name_encoder
    , expect = Http.expectString ToggledActivation
    , timeout = Nothing
    , tracker = Nothing
    }


deactivate model policy =
    let policy_name_encoder =
            [ ("name" , E.string policy.name) ]
    in
    Http.request
    { url = UB.crossOrigin model.baseurl
          [ "unschedule-policy" ]
          [ UB.string "name" policy.name ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_name_encoder
    , expect = Http.expectString ToggledActivation
    , timeout = Nothing
    , tracker = Nothing
    }


refreshnow model policy =
    let policy_name_encoder =
            [ ("name" , E.string policy.name) ]
    in
    Http.request
    { url = UB.crossOrigin model.baseurl
          [ "api/cache/refresh-policy-now" ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.jsonBody <| E.object policy_name_encoder
    , expect = Http.expectWhatever RefreshNowAsked
    , timeout = Nothing
    , tracker = Nothing
    }


type Msg
    = GotPolicies (Result Http.Error (List Policy))
    | AskDeletePolicy String
    | CancelDeletePolicy
    | DeletePolicy String
    | DeletedPolicy (Result Http.Error String)
    | NewPolicy
    | EditPolicy Policy
    | PolicyField Policy String String
    | ValidatedPolicy (Result Http.Error String)
    | CreatePolicy
    | CreatedPolicy (Result Http.Error String)
    | UpdatePolicy
    | UpdatedPolicy (Result Http.Error String)
    | CancelPolicyCreation
    | CancelPolicyEdition
    | LinkPolicySeries Policy
    | RefreshNow Policy
    | RefreshNowAsked (Result Http.Error ())
    | GotCachedSeries (Result Http.Error (List String))
    | GotFreeSeries (Result Http.Error (List String))
    | AddToCache String
    | RemoveFromCache String
    | CachedSeriesQuery String
    | CachedSeriesFormulaQuery String
    | FreeSeriesQuery String
    | FreeSeriesFormulaQuery String
    | CancelLink
    | ValidateLink
    | CacheWasSet (Result Http.Error String)
    | CacheWasUnset (Result Http.Error String)
    | ToggleActivation Policy
    | ToggledActivation (Result Http.Error String)


update_policy_field policy fieldname value =
    case fieldname of
        "name" -> { policy | name = value }
        "initial_revdate" -> { policy | initial_revdate = value }
        "look_before" -> { policy | look_before = value }
        "look_after" -> { policy | look_after = value }
        "revdate_rule" -> { policy | revdate_rule = value }
        "schedule_rule" -> { policy | schedule_rule = value }
        _ -> policy


mode model =
    case model.adding of
        Nothing ->
            case model.editing of
                Nothing -> "unknown"
                Just _ -> "edit"
        Just _ -> "create"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPolicies (Ok policies) ->
            ( { model | policies = policies }, Cmd.none )

        GotPolicies (Err err) ->
            nocmd <| model

        -- deletion
        AskDeletePolicy name ->
            ( { model | deleting = Just name }
            , Cmd.none
            )

        CancelDeletePolicy ->
            ( { model | deleting = Nothing }
            , Cmd.none
            )

        DeletePolicy name ->
            ( model, deletepolicy model name )

        DeletedPolicy _ ->
            ( model, getpolicies model )

        -- addition
        NewPolicy ->
            ( { model | adding = Just <| Policy "" False "" "" "" "" "" False }
            , Cmd.none
            )

        PolicyField policy field value ->
            let
                updated = update_policy_field policy field value
                newmodel =
                    if (mode model) == "edit" then
                        { model
                            | editing = Just <| if field == "name" then policy else updated
                        }
                    else
                        { model | adding = Just <| updated }
            in
            ( newmodel
            , validatepolicy newmodel updated
            )

        ValidatedPolicy (Ok val) ->
            let
                newmodel =
                    if String.startsWith "{}" val
                    then { model | editerror = Nothing }
                    else
                        case D.decodeString policy_error_decoder val of
                            Ok polerror -> { model | editerror = Just polerror }
                            Err err -> model
            in
            nocmd newmodel

        ValidatedPolicy (Err err) ->
            nocmd model

        CreatePolicy ->
            case model.adding of
                Nothing -> nocmd model
                Just policy ->
                    ( model, sendpolicy model policy )

        CreatedPolicy (Ok _) ->
            ( { model | editerrormsg = "", adding = Nothing }
            , getpolicies model
            )

        CreatedPolicy (Err err) ->
            let emsg = unwraperror err in
            nocmd { model | editerrormsg = emsg }

        CancelPolicyCreation ->
            nocmd { model
                      | editerror = Nothing
                      , editerrormsg = ""
                      , adding = Nothing
                  }

        -- edition
        EditPolicy policy ->
            nocmd { model | editing = Just policy }

        UpdatePolicy ->
            case model.editing of
                Nothing -> nocmd model
                Just policy ->
                    ( model, updatepolicy model policy )

        UpdatedPolicy (Ok _) ->
            ( { model | editerrormsg = "", editing = Nothing }
            , getpolicies model
            )

        UpdatedPolicy (Err err) ->
            let emsg = unwraperror err in
            nocmd { model | editerrormsg = emsg }

        CancelPolicyEdition ->
            nocmd { model
                      | editerror = Nothing
                      , editerrormsg = ""
                      , editing = Nothing
                  }

        -- link to series
        LinkPolicySeries policy ->
            ( { model | linking = Just policy }
            , Cmd.batch
                [ getcachedseries model policy
                , getfreeseries model policy
                ]
            )

        GotCachedSeries (Ok cachedseries) ->
            nocmd { model | cachedseries = cachedseries }

        GotCachedSeries (Err err) ->
            nocmd <| model

        GotFreeSeries (Ok freeseries) ->
            nocmd { model | freeseries = freeseries }

        GotFreeSeries (Err err) ->
            nocmd <| model

        AddToCache series ->
            let waspending = Set.member series model.removefromcache in
            nocmd <| { model
                         | addtocache = if waspending then
                                            model.addtocache
                                        else
                                            Set.insert series model.addtocache
                         , removefromcache = Set.remove series model.removefromcache
                         , freeseries = List.filter (\x -> x /= series) model.freeseries
                         , cachedseries = List.sort <| List.append model.cachedseries [ series ]
                     }

        RemoveFromCache series ->
            let waspending = Set.member series model.addtocache in
            nocmd <| { model
                         | addtocache = Set.remove series model.addtocache
                         , removefromcache = if waspending then
                                                 model.removefromcache
                                             else
                                                 Set.insert series model.removefromcache
                         , freeseries = List.sort <| List.append model.freeseries [ series ]
                         , cachedseries = List.filter (\x -> x /= series) model.cachedseries
                     }

        CachedSeriesQuery filter ->
            nocmd { model | cachedseriesquery = filter }

        CachedSeriesFormulaQuery filter ->
            nocmd { model | cachedseriesformulaquery = filter }

        FreeSeriesQuery filter ->
            nocmd { model | freeseriesquery = filter }

        FreeSeriesFormulaQuery filter ->
            nocmd { model | freeseriesformulaquery = filter }

        CancelLink ->
            nocmd <| { model
                         | addtocache = Set.empty
                         , removefromcache = Set.empty
                         , cachedseries = []
                         , freeseries = []
                         , linking = Nothing
                     }

        ValidateLink ->
            case model.linking of
                Nothing -> nocmd model
                Just policy ->
                    let
                        set = setcache model policy.name
                        unset = unsetcache model
                    in
                    ( { model
                          | addtocache = Set.empty
                          , removefromcache = Set.empty
                          , cachedseries = []
                          , cachedseriesquery = ""
                          , freeseries = []
                          , linking = Nothing
                      }
                    , Cmd.batch <| List.concat
                        [ List.map set (Set.toList model.addtocache)
                        , List.map unset (Set.toList model.removefromcache)
                        ]
                    )

        CacheWasSet _ -> nocmd model
        CacheWasUnset _ -> nocmd model

        -- activation

        ToggleActivation policy ->
            case policy.active of
                True -> (model
                        , deactivate model policy
                        )
                False -> (model
                         , activate model policy
                         )

        ToggledActivation (Ok _) ->
            ( model , getpolicies model )

        ToggledActivation (Err _) -> nocmd model

        -- refresh

        RefreshNow pol ->
            ( model
            , refreshnow model pol
            )

        RefreshNowAsked _ ->
            nocmd model


viewdeletepolicyaction model policy =
    let askdelete =
            [ H.button [ HA.class "btn btn-outline-danger"
                       , HA.type_ "button"
                       , HE.onClick (AskDeletePolicy policy.name)
                       ]
                  [ H.text "delete" ]
            ]
    in case model.deleting of
           Nothing -> askdelete
           Just name ->
               if name == policy.name then
                   [H.button [ HA.class "btn btn-warning"
                             , HA.type_ "button"
                             , HE.onClick CancelDeletePolicy
                             ]
                        [ H.text "cancel" ]
                   , H.button [ HA.class "btn btn-success"
                              , HA.type_ "button"
                              , HE.onClick (DeletePolicy name)
                              ]
                       [ H.text "confirm" ]
                   ]
                   else askdelete


viewactivatepolicyaction model policy =
    [ H.button [ HA.class <| if policy.active then "btn btn-warning" else "btn btn-success"
               , HA.type_ "button"
               , HE.onClick (ToggleActivation policy)
               ]
          [ H.text <| if policy.active then "deactivate" else "activate" ]
    ]


viewforcerefreshaction model policy =
    if policy.active then
        [ H.button [ HA.class "btn btn-info"
                   , HA.type_ "button"
                   , HE.onClick (RefreshNow policy)
                   , HA.title "force an immediate policy refresh"
                   ]
              [ H.text "refresh" ]
        ]
    else []


vieweditpolicyaction model policy =
    if policy.active then [] else
    [ H.button [ HA.class "btn btn-primary"
               , HA.type_ "button"
               , HE.onClick (EditPolicy policy)
               ]
          [ H.text "edit" ]
    ]


viewpolicy model policy =
    H.tr [  HA.class "gridded_policy" ]
        [ H.td []
              [ H.a [ HA.href "#"
                    , HE.onClick (LinkPolicySeries policy)
                    ]
                    [ H.text policy.name ]
              ]
        , H.td [] [ H.text <| if policy.ready then "true" else "false" ]
        , H.td [] [ H.text <| policy.initial_revdate ]
        , H.td [] [ H.text <| policy.look_before ]
        , H.td [] [ H.text <| policy.look_after ]
        , H.td [] [ H.text <| policy.revdate_rule ]
        , H.td [] [ H.text <| policy.schedule_rule ]
        , H.div [] <|
            (viewactivatepolicyaction model policy)
            ++
            (vieweditpolicyaction model policy)
            ++
            (viewforcerefreshaction model policy)
            ++
            (viewdeletepolicyaction model policy)
        ]


haserror editerror fieldname =
    case editerror of
        Nothing -> False
        Just polerror ->
            case fieldname of
                "initial_revdate" ->
                    case polerror.initial_revdate of
                        Nothing -> False
                        _ -> True
                "look_before" ->
                    case polerror.look_before of
                        Nothing -> False
                        _ -> True
                "look_after" ->
                    case polerror.look_after of
                        Nothing -> False
                        _ -> True
                "revdate_rule" ->
                    case polerror.revdate_rule of
                        Nothing -> False
                        _ -> True
                "schedule_rule" ->
                    case polerror.schedule_rule of
                        Nothing -> False
                        _ -> True
                _ -> False


inputs =
    [ ("name", "name", "policy name" )
    , ("initial_revdate", "initial revision date", "e.g. (date \"2022-1-1\")" )
    , ("look_before", "look before", "e.g. (shifted now #:days -15)" )
    , ("look_after", "look after", "e.g. (shifted now #:days 15)" )
    , ("revdate_rule", "revision date rule", "in crontab format" )
    , ("schedule_rule", "schedule rule", "in crontab format" )
    ]



polget pol name =
    case name of
        "name" -> pol.name
        "initial_revdate" -> pol.initial_revdate
        "look_before" -> pol.look_before
        "look_after" -> pol.look_after
        "revdate_rule" -> pol.revdate_rule
        "schedule_rule" -> pol.schedule_rule
        _ -> ""


makeinput model policy (fieldname, displayname, placeholder) =
    [ H.label
          ([ HA.for fieldname] ++ if haserror model.editerror fieldname
                                  then [ HA.class "field_error" ]
                                  else [])
          [ H.text displayname ]
    , H.input
        [ HA.class "form-control"
        , HA.placeholder placeholder
        , HE.onInput (PolicyField policy fieldname)
        , HA.value <| polget policy fieldname
        ] []
    ]


editpolicyform model pol basetext actiontext doitmsg cancelmsg =
    let
        editor =
            case model.editerror of
                Nothing -> [ HE.onClick doitmsg ]
                Just polerror -> [ HA.disabled True ]

    in
    H.div []
        [ H.h3 [] [ H.text basetext  ]
        , H.button ([ HA.class "btn btn-success"
                    , HA.type_ "button"
                    ] ++ editor)
            [ H.text actiontext ]
        , H.button [ HA.class "btn btn-warning"
                   , HA.type_ "button"
                   , HE.onClick cancelmsg
                   ]
            [ H.text "cancel" ]
        , H.p [] [ H.text model.editerrormsg ]
        , H.form [] <| ( List.concat <| List.map (makeinput model pol) inputs )
        ]


editpolicy model =
    case model.editing of
        Nothing -> H.div [] []
        Just policy ->
            editpolicyform
                model
                policy
                "Edit a formula cache policy"
                "edit"
                UpdatePolicy
                CancelPolicyEdition


newpolicy model =
    case model.adding of
        Nothing -> H.div [] []
        Just policy ->
            editpolicyform
            model
            policy
            "Create a fresh formula cache policy"
            "create"
            CreatePolicy
            CancelPolicyCreation


filterbywords filterme query =
    let
        querywords =
            String.words query
        filterstep word wordlist =
            List.filter (\item -> String.contains word item) wordlist
        filterall words wordlist =
            case words of
                [] -> wordlist
                head::tail -> filterall tail <| filterstep head wordlist
    in filterall querywords filterme


viewseriesinlist model text event name  =
    H.li []
        [ H.button [ HA.class "btn btn-success"
                    , HA.type_ "button"
                    , HE.onClick <| event name
                    ]
            [ H.text text ]
        , H.a
            [ HA.href <| UB.crossOrigin model.baseurl
                  [ "tsinfo" ]
                  [ UB.string "name" name ]
            , HA.target "_blank"
            ]
            [ H.text name ]
        ]


viewcachedserieslist model =
    H.div []
        [ H.h5 [] [ H.text "Cached series" ]
        , H.p [] [ H.input [ HA.class "form-control"
                           , HE.onInput CachedSeriesQuery
                           , HA.placeholder "type here to filter the series list by name"
                           ] []
                 ]
        , H.p [] [ H.input [ HA.class "form-control"
                           , HE.onInput CachedSeriesFormulaQuery
                           , HA.placeholder "type here to filter the series list by formula contents"
                           ] []
                 ]
        , H.ul [] <|
            List.map
                (viewseriesinlist model "remove" RemoveFromCache)
                (filterbywords model.cachedseries model.cachedseriesquery)
        ]


viewfreeserieslist model =
    H.div [ ]
        [ H.h5 [] [ H.text "Free series" ]
        , H.p [] [ H.input [ HA.class "form-control"
                           , HE.onInput FreeSeriesQuery
                           , HA.placeholder "type here to filter the series list by name"
                           ] []
                 ]
        , H.p [] [ H.input [ HA.class "form-control"
                           , HE.onInput FreeSeriesFormulaQuery
                           , HA.placeholder "type here to filter the series list by formula contents"
                           ] []
                 ]
        , H.ul [] <|
            List.map
                (viewseriesinlist model "add" AddToCache)
                (filterbywords model.freeseries model.freeseriesquery)
        ]


viewlinkpolicy model policy =
    H.div []
        [ H.h3 [] [ H.text ("Link policy " ++ policy.name) ]
        , if (not <| Set.isEmpty model.addtocache) ||
             (not <| Set.isEmpty model.removefromcache) then
              H.button [ HA.class "btn btn-success"
                   , HA.type_ "button"
                   , HE.onClick ValidateLink
                   ]
            [ H.text "apply" ]
          else
              H.span [] []
        , H.button [ HA.class "btn btn-warning"
                   , HA.type_ "button"
                   , HE.onClick CancelLink
                   ]
            [ H.text "exit" ]
        , H.div [ HA.class "link_policy" ]
            [ viewcachedserieslist model
            , viewfreeserieslist model
            ]
        ]


viewpoliciesheader =
    let columns =
            [ "name", "ready", "initial revision date"
            , "look before", "look after"
            , "rev date rule", "schedule rule", "actions"
            ]
    in H.tr
        [ HA.class "gridded_policy" ]
        <| List.map (\item -> H.th [] [ H.text item ]) columns


viewpolicies model =
    case model.adding of
        Nothing ->
            case model.editing of
                Nothing ->
                    case model.linking of
                        Nothing ->
                            H.div []
                                [ H.button [ HA.class "btn btn-primary"
                                           , HA.type_ "button"
                                           , HE.onClick NewPolicy
                                           ]
                                      [ H.text "create a cache policy" ]
                                , H.table [ HA.class "policy_list" ]
                                    <| (++)
                                        [ H.thead [] [ viewpoliciesheader ] ]
                                        [ H.tbody
                                              []
                                              <| List.map (viewpolicy model) model.policies
                                        ]
                                ]
                        Just policy ->
                            viewlinkpolicy model policy
                Just policy ->
                    editpolicy model
        Just policy ->
            newpolicy model


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "Policies" ]
        , viewpolicies model
        ]


sub model = Sub.none


type alias Input =
    { baseurl : String }

main : Program Input Model Msg
main =
    let
        init input =
            let model = Model
                        input.baseurl
                        []
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        ""
                        Nothing
                        []
                        ""
                        ""
                        []
                        ""
                        ""
                        Set.empty
                        Set.empty
            in
            ( model
            , getpolicies model
            )
    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
