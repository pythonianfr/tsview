module Search exposing (main)

import Browser
import Catalog as Cat
import Catalog exposing (Msg(..))
import Debouncer.Messages as Debouncer exposing
    (Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Html.Events as HE
import Html.Keyed as K
import Html.Lazy as L
import Http
import Json.Decode as D
import List.Extra as LE
import Metadata as M
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type Mode =
    Series | Groups


type alias Model =
    { baseurl : String
    , mode : Mode
    -- base catalog elements
    , catalog : Cat.Model
    , seriesmetadata : Dict String (Dict String M.MetaVal)
    , seriesformula : Dict String String
    , groupsmetadata : Dict String (Dict String M.MetaVal)
    , groupsformula : Dict String String
    -- filtered items
    , filteredseries : List String
    , filteredgroups : List String
    -- filter state (series)
    , selectedserieskinds : List String
    , selectedseriessources : List String
    -- filter state (series)
    , selectedgroupskinds : List String
    , selectedgroupssources : List String
    -- filter state (all)
    , filterbyname : Maybe String
    , filterbyformula : Maybe String
    -- filter state/metadata
    , metaitem : (String, String)
    , filterbymeta : List (String, String)
    , errors : List String
    -- debouncing
    , namefilterdeb : Debouncer Msg
    , formulafilterdeb : Debouncer Msg
    }


type Msg
    = GotCatalog Cat.Msg
    | GotSeriesMeta (Result Http.Error String)
    | GotGroupsMeta (Result Http.Error String)
    | GotAllSeriesFormula (Result Http.Error String)
    | GotAllGroupsFormula (Result Http.Error String)
    | NameFilter String
    | FormulaFilter String
    | KindUpdated String
    | SourceUpdated String
    -- metadata filter
    | NewValue String
    | NewKey String
    | AddMetaItem
    | MetaItemToDelete (String, String)
    -- debouncer
    | DebounceNameFilter (Debouncer.Msg Msg)
    | DebounceFormulaFilter (Debouncer.Msg Msg)
    -- mode
    | ToggleMode


getmeta baseurl dtype event =
    Http.get
        { expect =
              Http.expectString event
        , url =
            UB.crossOrigin baseurl
                [ if dtype == "series" then "tssearch" else "groupsearch"
                , "allmetadata"
                ] []
        }


decodemeta allmeta =
    let
        all = D.dict (D.dict M.decodemetaval)
    in
    D.decodeString all allmeta


decodeformulae allformula =
    let
        all = D.dict D.string
    in
    D.decodeString all allformula


insert list item =
    List.append list [item]


remove list item =
    List.filter ((/=) item) list


-- filters

nullfilter model =
        { model
            | filteredseries = List.sort model.catalog.series
            , filteredgroups = List.sort model.catalog.groups
        }


namefilter model =
    case model.filterbyname of
        Nothing -> model
        Just match ->
            { model
                | filteredseries = List.filter (U.fragmentsmatcher match) model.filteredseries
                , filteredgroups = List.filter (U.fragmentsmatcher match) model.filteredgroups
            }


formulafilter model =
    case model.filterbyformula of
        Nothing -> model
        Just match ->
            { model
                | filteredseries = U.filterbyformula model.seriesformula model.filteredseries match
                ,  filteredgroups = U.filterbyformula model.groupsformula model.filteredgroups match
            }


catalogfilter data authority keys =
    if keys == Dict.keys authority then data else
    let
        dataforkey key =
                Set.toList
                    <| Maybe.withDefault Set.empty
                    <| Dict.get key authority
        alldata =
            Set.fromList <| List.concat <| List.map dataforkey keys
    in
    List.filter (\item -> (Set.member item alldata)) data


sourcefilter model =
    { model
        | filteredseries = catalogfilter
                           model.filteredseries
                           model.catalog.seriesbysource
                           model.selectedseriessources
        , filteredgroups = catalogfilter
                           model.filteredgroups
                           model.catalog.groupsbysource
                           model.selectedgroupssources
    }


kindfilter model =
    { model
        | filteredseries = catalogfilter
                           model.filteredseries
                           model.catalog.seriesbykind
                           model.selectedserieskinds
        , filteredgroups = catalogfilter
                           model.filteredgroups
                           model.catalog.groupsbykind
                           model.selectedgroupskinds
    }


lower str =
    String.toLower str


filternothing list =
    List.filterMap identity list


metafilter model =
    if List.length model.filterbymeta == 0 then model else
        let
            lowerkeys metadict =
                List.map lower <| Dict.keys metadict

            lowervalue  =
                M.metavaltostring >> lower

            lowervalues metadict =
                List.map lowervalue <| Dict.values metadict

            matchkeys key metadict =
                let lkey = lower key in
                List.any (\x -> String.contains lkey x) <| lowerkeys metadict

            matchvalue val metaval =
                String.contains val metaval

            matchvalues value meta =
                let lvalue = lower value in
                List.any (matchvalue lvalue) <| lowervalues meta

            match meta query =
                case query of
                    (key, "") ->
                        matchkeys key meta

                    ("", value) ->
                        matchvalues value meta

                    (key, value) ->
                        (matchkeys key meta) && (matchvalues value meta)

            metadata =
                case model.mode of
                    Series -> model.seriesmetadata
                    Groups -> model.groupsmetadata

            bymeta name =
                case Dict.get name metadata of
                    Nothing -> False
                    Just meta ->
                        List.all (match meta) model.filterbymeta
        in
        case model.mode of
            Series ->
                { model | filteredseries = List.filter bymeta model.filteredseries }
            Groups ->
                { model | filteredgroups = List.filter bymeta model.filteredgroups }


allfilters model =
    model |> nullfilter
             >> sourcefilter
             >> kindfilter
             >> namefilter
             >> formulafilter
             >> metafilter

-- debouncing

updatednamefilterbouncer =
    { mapMsg = DebounceNameFilter
    , getDebouncer = .namefilterdeb
    , setDebouncer = \deb model -> { model | namefilterdeb = deb }
    }


updatedformulafilterbouncer =
    { mapMsg = DebounceFormulaFilter
    , getDebouncer = .formulafilterdeb
    , setDebouncer = \deb model -> { model | formulafilterdeb = deb }
    }



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog catmsg ->
            let
                cat = Cat.update catmsg model.catalog
                m2 = { model | catalog = cat }
                ( newmodel, cmds ) = case catmsg of
                  ReceivedSeries _ ->
                      ( { m2
                            | filteredseries = cat.series
                            , selectedserieskinds = Dict.keys cat.seriesbykind
                            , selectedseriessources = Dict.keys cat.seriesbysource
                        }
                      , [ getmeta model.baseurl "series" GotSeriesMeta
                        , U.getformulas model.baseurl "series" GotAllSeriesFormula
                        ]
                      )
                  ReceivedGroups _ ->
                      ( { m2
                          | filteredgroups = cat.groups
                          , selectedgroupskinds = Dict.keys cat.groupsbykind
                          , selectedgroupssources = Dict.keys cat.groupsbysource
                        }
                      , [ getmeta model.baseurl "groups" GotGroupsMeta
                        , U.getformulas model.baseurl "groups" GotAllGroupsFormula
                        ]
                      )
            in
            ( newmodel
            , Cmd.batch cmds
            )

        GotSeriesMeta (Ok rawmeta) ->
            case decodemeta rawmeta of
                Ok meta ->
                    U.nocmd { model | seriesmetadata = meta }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotSeriesMeta (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        GotGroupsMeta (Ok rawmeta) ->
            case decodemeta rawmeta of
                Ok meta ->
                    U.nocmd { model | groupsmetadata = meta }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotGroupsMeta (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        GotAllSeriesFormula (Ok rawformulae) ->
            case decodeformulae rawformulae of
                Ok formulae ->
                    U.nocmd { model | seriesformula = formulae }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotAllSeriesFormula (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        GotAllGroupsFormula (Ok rawformulae) ->
            case decodeformulae rawformulae of
                Ok formulae ->
                    U.nocmd { model | groupsformula = formulae }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotAllGroupsFormula (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        NameFilter value ->
            let
                filter = if value /= "" then Just value else Nothing
                newmodel = { model | filterbyname = filter }
            in
            U.nocmd <| allfilters newmodel

        FormulaFilter value ->
            let
                filter = if value /= "" then Just value else Nothing
                newmodel = { model | filterbyformula = filter }
            in
            U.nocmd <| allfilters newmodel

        KindUpdated kind ->
            let
                newserieskinds =
                    if List.member kind model.selectedserieskinds
                    then remove model.selectedserieskinds kind
                    else insert model.selectedserieskinds kind

                newgroupkinds =
                    if List.member kind model.selectedgroupskinds
                    then remove model.selectedgroupskinds kind
                    else insert model.selectedgroupskinds kind

                newmodel =
                    { model
                        | selectedserieskinds = List.sort newserieskinds
                        , selectedgroupskinds = List.sort newgroupkinds
                    }
            in
            U.nocmd <| allfilters newmodel

        SourceUpdated source ->
            let
                newseriessources =
                    if List.member source model.selectedseriessources
                    then remove model.selectedseriessources source
                    else insert model.selectedseriessources source

                newgroupsources =
                    if List.member source model.selectedgroupssources
                    then remove model.selectedgroupssources source
                    else insert model.selectedgroupssources source

                newmodel =
                    { model
                        | selectedseriessources = newseriessources
                        , selectedgroupssources = newgroupsources
                    }
            in
            U.nocmd <| allfilters newmodel

        -- metadata filtering

        NewValue value ->
            U.nocmd { model | metaitem = (U.first model.metaitem, value) }

        NewKey value ->
            U.nocmd { model | metaitem = (value, U.snd model.metaitem) }

        AddMetaItem ->
            if model.metaitem == ("", "") then U.nocmd model else
            let
                newmodel = { model
                               | metaitem = ("", "")
                               , filterbymeta = List.append model.filterbymeta [model.metaitem]
                           }
           in
           U.nocmd <| allfilters newmodel

        MetaItemToDelete (key, value) ->
            U.nocmd <| allfilters
                { model
                    | filterbymeta = List.filter (\x -> x /= (key, value)) model.filterbymeta
                }

        -- Debouncing

        DebounceNameFilter val ->
            Debouncer.update update updatednamefilterbouncer val model

        DebounceFormulaFilter val ->
            Debouncer.update update updatedformulafilterbouncer val model

        -- Mode

        ToggleMode ->
            let
                mode =
                    case model.mode of
                        Series -> Groups
                        Groups -> Series
             in
             U.nocmd { model | mode = mode }


viewnamefilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter by name"
                , A.size 80
                , A.title "You can type name fragments separated by spaces"
                , HE.onInput NameFilter
                ] []
    in
    H.map (provideInput >> DebounceNameFilter) input


viewformulafilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter on formula content"
                , A.size 80
                , A.title "You can type series name fragments and formula operator names"
                , HE.onInput FormulaFilter
                ] []
    in
    H.map (provideInput >> DebounceFormulaFilter) input


selectedkinds model =
    case model.mode of
        Series -> model.selectedserieskinds
        Groups -> model.selectedgroupskinds


kinds model =
    case model.mode of
        Series -> Dict.keys model.catalog.seriesbykind
        Groups -> Dict.keys model.catalog.groupsbykind


viewkindfilter model =
    let
        checkbox kind =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series type"]
                [ H.input
                    [ A.attribute "type" "checkbox"
                    , A.class "form-check-input"
                    , A.value kind
                    , A.checked <| List.member kind <| selectedkinds model
                    , HE.onClick <| KindUpdated kind
                    ] []
                , H.label
                    [ A.class "form-check-label"
                    , A.for kind ]
                    [ H.text kind ]
                ]
    in
    H.div [] <|
        H.span
            [ A.class "font-italic" ]
            [ H.text "series kind → " ] :: (List.map checkbox <| kinds model)


bysources model =
    case model.mode of
        Series -> Dict.keys model.catalog.seriesbysource
        Groups -> Dict.keys model.catalog.groupsbysource


selectedsources model =
    case model.mode of
        Series -> model.selectedseriessources
        Groups -> model.selectedgroupssources


viewsourcefilter model =
    let
        sources = bysources model
        checkbox source =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series source"]
                [ H.input
                      [ A.attribute "type" "checkbox"
                      , A.class "form-check-input"
                      , A.value source
                      , A.checked <| List.member source <| selectedsources model
                      , HE.onClick <| SourceUpdated source
                      ] []
                , H.label
                      [ A.class "form-check-label"
                      , A.for source ]
                      [ H.text source ]
                ]
    in
    if List.length sources > 1
    then H.div []  <|
        H.span
            [ A.class "font-italic" ]
            [ H.text "series sources → " ] :: (List.map checkbox sources)
    else H.span [] []


viewmetafilter model =
    let
        addentry =
            H.button
                [ A.attribute "type" "button"
                , A.title "add the metadata filter rule"
                , A.class "btn btn-primary btn-sm"
                , HE.onClick AddMetaItem
                ]
                [ H.text "add" ]
        delete key value =
            H.button
                [ A.attribute "type" "button"
                , A.title "remove the metadata filter rule"
                , A.class "btn btn-warning btn-sm"
                , HE.onClick (MetaItemToDelete (key, value))
                ]
                [ H.text "delete" ]
        fields k v deleteaction keycb valuecb =
            H.div []
                [ H.input
                      ([ A.attribute "type" "text"
                       , A.class "form-control-sm"
                       , A.placeholder "filter by metadata key"
                       , A.value k
                       ] ++ case keycb of
                                Nothing -> []
                                Just cb -> [ cb ]
                      ) []
                , H.span [] [ H.text " " ]
                , H.input
                    ([ A.attribute "type" "text"
                     , A.class "form-control-sm"
                     , A.placeholder "filter by metadata value"
                     , A.value v
                     ] ++ case valuecb of
                              Nothing -> []
                              Just cb -> [ cb ]
                    ) []
                , H.span [] [ H.text " " ]
                , case deleteaction of
                      Nothing -> addentry
                      Just action -> action k v
                ]
    in
    H.div
        [ ]
        ([ fields
               (U.first model.metaitem)
               (U.snd model.metaitem)
               Nothing
               (Just <| HE.onInput NewKey)
               (Just <| HE.onInput NewValue)
         ] ++ List.map
             (\x -> fields (U.first x) (U.snd x) (Just delete) Nothing Nothing)
             model.filterbymeta
        )


viewfilteredqty model =
    let
        len =
            case model.mode of
                Series -> List.length model.filteredseries
                Groups -> List.length model.filteredgroups

        msg = if len == 0
              then "No item"
              else
                  (if len == 1
                   then "1 item"
                   else String.fromInt len ++ " items")
        msg2 = if len > 1000 then msg ++ " (only first 1000 items shown)." else msg ++ "."
    in
    H.p [] [ H.text ("Found " ++ msg2) ]


findkeysofvalue out map keys value justone =
    case keys of
        head::tail ->
            if Set.member value <| Maybe.withDefault Set.empty (Dict.get head map)
            then if justone then [head] else findkeysofvalue (head::out) map tail value justone
            else findkeysofvalue out map tail value justone

        [] -> out


datakind mode name catalog =
    let
        bykind =
            case mode of
                Series -> catalog.seriesbykind
                Groups -> catalog.groupsbykind
    in
    Maybe.withDefault "unknown" <|
        List.head <|
            findkeysofvalue [] bykind (Dict.keys bykind) name True


datasources mode name catalog =
    let
        bysource =
            case mode of
                Series -> catalog.seriesbysource
                Groups -> catalog.groupsbysource
    in
    findkeysofvalue [] bysource (Dict.keys bysource) name False


viewfiltered baseurl mode filtered catalog showsource filtersources =
    let
        item elt =
            let kind =
                    datakind mode elt catalog
                sources =
                    List.filter
                        (\src -> List.member src filtersources)
                        (datasources mode elt catalog)
            in
            (elt, H.li
                 [ A.class "list-group-item p-1" ]
                 [ H.span
                       [ A.class <|
                             case kind of
                                 "formula" ->  "badge badge-success"
                                 "bound" -> "badge badge-info"
                                 _ -> "badge badge-secondary"
                       ]
                       [ H.text kind ]
                 , H.span [] [ H.text " " ]
                 , H.a [ A.href (UB.crossOrigin
                                     baseurl
                                     [ case mode of
                                           Series -> "tsinfo"
                                           Groups -> "groupinfo"
                                     ]
                                     [ UB.string "name" elt ]
                                )
                       ]
                       [ H.text elt ]
                 , if showsource
                   then
                       H.span
                           -- alas, Chrome does not know `inline-end`
                           [ A.style "float" "right" ]
                           <| List.map
                               (\source ->
                                    H.span []
                                    [ H.span [] [ H.text " " ]
                                    , H.span
                                        [ A.class "badge badge-info" ]
                                        [ H.text source ]
                                    ]
                               )
                               sources
                   else
                       H.span [] []
                 ]
            )
        ufiltered = LE.unique filtered

        filteredslice =
            List.take 1000 (List.sort ufiltered)

        missing =
            (List.length filteredslice) < (List.length ufiltered)

        items =
            List.map item filteredslice

        noseries = (List.length ufiltered) == 0

    in
    K.node "ul"
        [ A.class "list-group list-group-flush" ]
        items


viewerrors model =
    if List.length model.errors > 0 then
    H.div []
        [ H.h2 [] [ H.text "Errors" ]
        , H.div [] (List.map (\x -> H.p [] [ H.text x ]) model.errors)
        ]
    else H.span [] []


-- menu

type alias Menu = List Section

type alias Section =
    { label: String
    , links: List Link
    }

type alias Link =
    { label: String
    , target: String }

contentMenu : Menu
contentMenu =
    [ { label = "Timeseries"
      , links = [ { label = "Catalog"
                  , target = "/tssearch" }
                , { label = "Quick View"
                  , target = "/tsview" }
                , { label = "Delete"
                  , target = "/tsdelete" }
                ]
      }
    , { label = "Formula"
      , links = [ { label = "Documentation"
                  , target = "/tsformula/operators" }
                , { label = "Catalog"
                  , target = "/formulas" }
                , { label = "Create"
                  , target = "/tsformula" }
                , { label = "Update batch"
                  , target = "/addformulas" }
                , { label = "Setup cache"
                  , target = "/formulacache" }
                ]
      }
    , { label = "Monitoring"
      , links = [ { label = "Tasks"
                  , target = "/tasks/" }
                , { label = "Series import"
                  , target = "/tswatch/" }
                ]
      }
    ]

displayContent : Menu -> H.Html Msg
displayContent content =
    H.ul
        []
        ( List.map
            ( \ section -> H.li
                            []
                            [ H.text section.label
                            , displayLinks section.links ] )
            content )

displayLinks : List Link -> H.Html Msg
displayLinks links =
        H.ul
        []
        ( List.map
            (\ link -> H.li
                            []
                            [ H.a
                                [ A.href link.target ]
                                [ H.text link.label ] ] )
            links )

viewMenu : H.Html Msg
viewMenu =
    displayContent contentMenu


view : Model -> H.Html Msg
view model =
    let
        nbsources =
            case model.mode of
                Series -> Dict.size model.catalog.seriesbysource
                Groups -> Dict.size model.catalog.groupsbysource

        mode =
            case model.mode of
                Series -> "Series"
                Groups -> "Groups"

        filtered =
            case model.mode of
                Series -> model.filteredseries
                Groups -> model.filteredgroups

    in
    H.div [ A.style "margin" ".5em" ]
        [ viewMenu
        ,   H.span
              [ HE.onClick ToggleMode
              , A.style "float" "right"
              ]
              [ H.div
                    [ A.class "form-check form-check-inline" ]
                    [ H.input [ A.class "form-check-input"
                              , A.type_ "radio"
                              , A.checked <| case model.mode of
                                                 Series -> True
                                                 Groups -> False
                              , A.name "doseries"
                              , A.id "doseries"] [ ]
                    , H.label
                        [ A.class "form-check-label"
                        , A.for "doseries"
                        ]
                        [ H.text "series" ]
                    ]
              , H.div [ A.class "form-check form-check-inline" ]
                  [ H.input  [ A.class "form-check-input"
                             , A.type_ "radio"
                             , A.checked <| case model.mode of
                                                Series -> False
                                                Groups -> True
                             , A.name "dogroups"
                             , A.id "dogroups" ] [ ]
                  , H.label
                      [ A.class "form-check-label"
                      , A.for "dogroups"
                      ]
                      [ H.text "groups" ]
                  ]
              ]
        , H.h1 [  ] [ H.text <| mode ++ " Catalog" ]
        , H.div
              [ A.class "tsview-form-input small" ]
              [ H.div [] [ viewnamefilter ]
              , H.div [] [ viewformulafilter ]
              , viewmetafilter model
              , viewkindfilter model
              , viewsourcefilter model
              , viewfilteredqty model
              ]
        , L.lazy6 viewfiltered
            model.baseurl model.mode filtered model.catalog (nbsources > 1) (selectedsources model)
        , viewerrors model
        ]


type alias Input =
    { baseurl : String }


main : Program Input  Model Msg
main =
       let
           debouncerconfig =
               Debouncer.manual |>
               settleWhenQuietFor (Just <| fromSeconds 0.3) |>
               toDebouncer

           newmodel input =
               Model
                   input.baseurl
                   Series
                   Cat.empty
                   Dict.empty
                   Dict.empty
                   Dict.empty
                   Dict.empty
                   []
                   []
                   []
                   []
                   []
                   []
                   Nothing
                   Nothing
                   ("", "")
                   []
                   []
                   debouncerconfig
                   debouncerconfig

           init input =
               ( newmodel input
               , Cmd.batch
                   [ Cmd.map GotCatalog <| Cat.get input.baseurl "series" 1 Cat.ReceivedSeries
                   , Cmd.map GotCatalog <| Cat.get input.baseurl "group" 1 Cat.ReceivedGroups
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
