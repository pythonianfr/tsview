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
    , filterbymeta : Dict Int (String, String)
    , errors : List String
    -- debouncing
    , namefilterdeb : Debouncer Msg
    , formulafilterdeb : Debouncer Msg
    , keyfilterdeb : Debouncer Msg
    , valuefilterdeb : Debouncer Msg
    , tzaware : String
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
    | NewValue Int String
    | NewKey Int String
    | AddMetaItem Int
    | MetaItemToDelete Int
    -- debouncer
    | DebounceNameFilter (Debouncer.Msg Msg)
    | DebounceFormulaFilter (Debouncer.Msg Msg)
    | DebounceKeyFilter (Debouncer.Msg Msg)
    | DebounceValueFilter (Debouncer.Msg Msg)
    -- mode
    | ToggleMode
    | Tzaware String


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


tzawarefilter : Model -> Model
tzawarefilter model =
    if model.tzaware == "any" then
        model
    else
        let
            getElement : Maybe M.MetaVal -> String
            getElement element =
                case element of
                    Just el ->
                        M.metavaltostring el
                    Nothing ->
                        ""
            tzFilter : List String -> Dict String (Dict String M.MetaVal) -> String -> List String
            tzFilter filtredNames dict tz =
                let
                    dictToTuple : (String, Dict String M.MetaVal) -> (String, String)
                    dictToTuple (key, smallDict) =
                        (key, getElement (Dict.get "tzaware" smallDict))
                    newList = List.map dictToTuple (Dict.toList dict)
                    listFiltred =
                        List.map
                            Tuple.first
                            (List.filter (\(_, bool) -> bool == tz) newList)
                in List.filter
                    (\element -> List.member element listFiltred) filtredNames

        in
        { model
            | filteredseries = tzFilter model.filteredseries model.seriesmetadata model.tzaware
            , filteredgroups = tzFilter model.filteredgroups model.groupsmetadata model.tzaware
        }


lower str =
    String.toLower str


filternothing list =
    List.filterMap identity list


metafilter model =
    if List.length (Dict.toList model.filterbymeta) == 0 then model else
        let
            filterbymeta = List.filter
                (\data -> data /= ("", ""))
                (Dict.values model.filterbymeta)
            lowerkeys metadict =
                List.map lower <| Dict.keys metadict

            lowervalue =
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

            matchkeyvalue key val meta =
                let
                    dval = Dict.get key meta
                in
                case dval of
                    Nothing ->
                        False
                    Just aval ->
                        (lowervalue aval) == (String.toLower val)

            match meta query =
                case query of
                    (key, "") ->
                        matchkeys key meta

                    ("", value) ->
                        matchvalues value meta

                    (key, value) ->
                        matchkeyvalue key value meta

            metadata =
                case model.mode of
                    Series -> model.seriesmetadata
                    Groups -> model.groupsmetadata

            bymeta name =
                case Dict.get name metadata of
                    Nothing -> False
                    Just meta ->
                        List.all (match meta) filterbymeta
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
             >> tzawarefilter

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


updatedkeyfilterbouncer =
    { mapMsg = DebounceKeyFilter
    , getDebouncer = .keyfilterdeb
    , setDebouncer = \deb model -> { model | keyfilterdeb = deb }
    }


updatedvaluefilterbouncer =
    { mapMsg = DebounceValueFilter
    , getDebouncer = .valuefilterdeb
    , setDebouncer = \deb model -> { model | valuefilterdeb = deb }
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

        NewValue inputId value ->
            let
                currentKey = U.first
                    (Maybe.withDefault
                        ("","")
                        (Dict.get inputId model.filterbymeta)
                    )
                newDict = Dict.insert
                    inputId
                    (currentKey, value)
                    model.filterbymeta
            in U.nocmd <| allfilters { model | filterbymeta = newDict }

        NewKey inputId key ->
            let
                currentValue = U.snd
                    (Maybe.withDefault
                        ("","")
                        (Dict.get inputId model.filterbymeta)
                    )
                newDict = Dict.insert
                    inputId
                    (key, currentValue)
                    model.filterbymeta
            in U.nocmd <| allfilters { model | filterbymeta = newDict }

        AddMetaItem inputId ->
            let
                newDict = Dict.insert
                    (inputId + 1)
                    ("", "")
                    model.filterbymeta
           in U.nocmd { model | filterbymeta = newDict }

        MetaItemToDelete inputId ->
            let
                newDict = Dict.remove inputId model.filterbymeta
            in
            U.nocmd <| allfilters { model | filterbymeta = newDict }

        -- Debouncing

        DebounceNameFilter val ->
            Debouncer.update update updatednamefilterbouncer val model

        DebounceFormulaFilter val ->
            Debouncer.update update updatedformulafilterbouncer val model

        DebounceKeyFilter val ->
            Debouncer.update update updatedkeyfilterbouncer val model

        DebounceValueFilter val ->
            Debouncer.update update updatedvaluefilterbouncer val model
        -- Mode

        ToggleMode ->
            let
                mode =
                    case model.mode of
                        Series -> Groups
                        Groups -> Series
             in
             U.nocmd { model | mode = mode }

        Tzaware value ->
            let
                newModel = { model | tzaware = value }
            in U.nocmd <| allfilters newModel


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


inputsFiled : String -> (Int, (String, String)) -> H.Html Msg
inputsFiled action metaData =
    let
        inputId = U.first metaData
        key = U.first ( U.snd metaData )
        value = U.snd ( U.snd metaData )
        keyInput = H.input
            [ A.attribute "type" "text"
                , A.class "form-control-sm"
                , A.placeholder "filter by metadata key"
                , A.value key
                , HE.onInput (NewKey inputId)
            ]
            []
        valueInput = H.input
            [ A.attribute "type" "text"
                , A.class "form-control-sm"
                , A.placeholder "filter by metadata value"
                , A.value value
                , HE.onInput (NewValue inputId)
            ]
            []
        addentry =
            H.button
                [ A.attribute "type" "button"
                , A.title "add the metadata filter rule"
                , A.class "btn btn-primary btn-sm"
                , HE.onClick (AddMetaItem inputId)
                ]
                [ H.text "add" ]
        delete =
            H.button
                [ A.attribute "type" "button"
                , A.title "remove the metadata filter rule"
                , A.class "btn btn-warning btn-sm"
                , HE.onClick (MetaItemToDelete inputId)
                ]
                [ H.text "delete" ]
    in  H.div
        [ A.id (String.fromInt inputId) ]
        [ H.map (provideInput >> DebounceKeyFilter) keyInput
        , H.span [] [ H.text " " ]
        ,  H.map (provideInput >> DebounceValueFilter) valueInput
        , H.span [] [ H.text " " ]
        , if action == "add" then addentry else delete
        ]


viewmetafilter : Model -> H.Html Msg
viewmetafilter model =
    let
        listData = List.reverse (Dict.toList model.filterbymeta)
        header = Maybe.withDefault (1, ("", "")) (List.head listData)
        tail = Maybe.withDefault [] (List.tail listData)

        in H.div
            [ ]
            ((inputsFiled "add" header):: List.map (inputsFiled "delete") tail)


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
    H.div [ A.class "main-content" ]
        [ H.span
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
        , H.h1 [ A.class "page-title" ] [ H.text <| mode ++ " Catalog" ]
        , H.div
              [ A.class "tsview-form-input" ]
              [ H.div [] [ viewnamefilter ]
              , H.div [] [ viewformulafilter ]
              , viewmetafilter model
              , tzawareDropdown model
              , viewkindfilter model
              , viewsourcefilter model
              , viewfilteredqty model
              ]
        , L.lazy6 viewfiltered
            model.baseurl model.mode filtered model.catalog (nbsources > 1) (selectedsources model)
        , viewerrors model
        ]


tzawareDropdown : Model -> H.Html Msg
tzawareDropdown model =
    let
        decodetzaware : String -> D.Decoder String
        decodetzaware tzaware =
            D.succeed tzaware

    in
    H.div
        [ ]
        [ H.text "Tzaware : "
        , H.select
            [ HE.targetValue
                |> D.andThen decodetzaware
                |> D.map Tzaware
                |> HE.on "change" ]
            (List.map (renderTzaware model.tzaware) ["any", "true", "false"])
        ]

renderTzaware : String -> String -> H.Html Msg
renderTzaware selectedTzaware tzaware =
    H.option
        [ A.value tzaware
        , A.selected (selectedTzaware == tzaware)
        ]
        [ H.text tzaware ]


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
                   Dict.empty
                   []
                   debouncerconfig
                   debouncerconfig
                   debouncerconfig
                   debouncerconfig
                   "any"

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
