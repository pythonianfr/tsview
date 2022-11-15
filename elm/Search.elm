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
    -- filter state
    , selectedkinds : List String
    , selectedsources : List String
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
    { model | filteredseries = List.sort model.catalog.series }


namefilter model =
    case model.filterbyname of
        Nothing -> model
        Just match ->
            { model | filteredseries =
                  List.filter (U.fragmentsmatcher match) model.filteredseries }


formulafilter model =
    case model.filterbyformula of
        Nothing -> model
        Just match ->
            { model | filteredseries =
                  U.filterbyformula model.seriesformula model.filteredseries match }


catalogfilter series authority keys =
    if keys == Dict.keys authority then series else
    let
        seriesforkey key =
                Set.toList
                    <| Maybe.withDefault Set.empty
                    <| Dict.get key authority
        allseries =
            Set.fromList <| List.concat <| List.map seriesforkey keys
    in
    List.filter (\item -> (Set.member item allseries)) series


sourcefilter model =
    { model | filteredseries =
          catalogfilter
          model.filteredseries
          model.catalog.seriesbysource
          model.selectedsources
    }


kindfilter model =
    { model | filteredseries =
          catalogfilter
          model.filteredseries
          model.catalog.seriesbykind
          model.selectedkinds
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
                        let
                            lvalue = lower value
                            lkey = lower key

                            keys =
                                List.filter
                                    (\x -> String.contains lkey x)
                                    (lowerkeys meta)
                            values =
                                List.map lowervalue <|
                                filternothing <| List.map (\x -> Dict.get x meta) keys

                        in List.any (matchvalue lvalue) values

            bymeta name =
                case Dict.get name model.seriesmetadata of
                    Nothing -> False
                    Just meta ->
                        List.all (match meta) model.filterbymeta
        in
        { model | filteredseries = List.filter bymeta model.filteredseries }


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
                            , selectedkinds = Dict.keys cat.seriesbykind
                            , selectedsources = Dict.keys cat.seriesbysource
                        }
                      , [ getmeta model.baseurl "series" GotSeriesMeta
                        , U.getformulas model.baseurl "series" GotAllSeriesFormula
                        ]
                      )
                  ReceivedGroups _ ->
                      ( { m2
                          | filteredgroups = cat.groups
                          , selectedkinds = Dict.keys cat.groupsbykind
                          , selectedsources = Dict.keys cat.groupsbysource
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
                newkinds =
                    if List.member kind model.selectedkinds
                    then remove model.selectedkinds kind
                    else insert model.selectedkinds kind
                newmodel = { model | selectedkinds = List.sort newkinds }
            in
            U.nocmd <| allfilters newmodel

        SourceUpdated source ->
            let
                newsources =
                    if List.member source model.selectedsources
                    then remove model.selectedsources source
                    else insert model.selectedsources source
                newmodel = { model | selectedsources = newsources }
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


viewkindfilter model =
    let
        kinds = Dict.keys model.catalog.seriesbykind
        checkbox kind =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series type"]
                [ H.input
                    [ A.attribute "type" "checkbox"
                    , A.class "form-check-input"
                    , A.value kind
                    , A.checked <| List.member kind model.selectedkinds
                    , HE.onClick <| KindUpdated kind
                    ] []
                , H.label
                    [ A.class "form-check-label"
                    , A.for kind ]
                    [ H.text kind ]
                ]
    in
    H.div [] <|
        H.span [ A.class "font-italic" ] [ H.text "series kind → " ] :: (List.map checkbox kinds)


viewsourcefilter model =
    let
        sources = Dict.keys model.catalog.seriesbysource
        checkbox source =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series source"]
                [ H.input
                      [ A.attribute "type" "checkbox"
                      , A.class "form-check-input"
                      , A.value source
                      , A.checked <| List.member source model.selectedsources
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
        H.span [ A.class "font-italic" ] [ H.text "series sources → " ] :: (List.map checkbox sources)
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
        len = List.length model.filteredseries
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


serieskind name catalog =
    Maybe.withDefault "unknown" <|
        List.head <|
            findkeysofvalue [] catalog.seriesbykind (Dict.keys catalog.seriesbykind) name True


seriessources name catalog =
    findkeysofvalue [] catalog.seriesbysource (Dict.keys catalog.seriesbysource) name False


viewfiltered baseurl filtered catalog showsource selectedsources =
    let
        item elt =
            let kind =
                    serieskind elt catalog
                sources =
                    List.filter
                        (\src -> List.member src selectedsources)
                        (seriessources elt catalog)
            in
            (elt, H.li
                 [ A.class "list-group-item p-1" ]
                 [ H.span
                       [ A.class (if kind == "formula"
                                  then "badge badge-success"
                                  else "badge badge-secondary")
                       ]
                       [ H.text kind ]
                 , H.span [] [ H.text " " ]
                 , H.a [ A.href (UB.crossOrigin
                                     baseurl
                                     [ "tsinfo" ]
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
            Dict.size model.catalog.seriesbysource

        mode =
            case model.mode of
                Series -> "Series"
                Groups -> "Groups"

    in
    H.div [ A.style "margin" ".5em" ]
        [ H.h1 [ HE.onClick ToggleMode ] [ H.text <| mode ++ " Catalog" ]
        , H.div
              [ A.class "tsview-form-input small" ]
              [ H.div [] [ viewnamefilter ]
              , H.div [] [ viewformulafilter ]
              , viewmetafilter model
              , viewkindfilter model
              , viewsourcefilter model
              , viewfilteredqty model
              ]
        , L.lazy5 viewfiltered
            model.baseurl model.filteredseries model.catalog (nbsources > 1) model.selectedsources
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
