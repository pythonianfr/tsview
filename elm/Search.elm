module Search exposing (main)

import Browser
import Catalog as Cat
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
import Html.Events exposing ( onClick
                            , onInput
                            , onSubmit
                            )
import Html.Keyed as K
import Html.Lazy as L
import Http
import Json.Decode as D
import Metadata as M
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    -- base catalog elements
    , catalog : Cat.Model
    , metadata : Dict String (Dict String M.MetaVal)
    , formula : Dict String String
    -- filtered series
    , filtered : List String
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
    | GotMeta (Result Http.Error String)
    | GotAllFormula (Result Http.Error String)
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


getmeta baseurl =
    Http.get
        { expect =
              Http.expectString GotMeta
        , url =
            UB.crossOrigin baseurl
                [ "tssearch", "allmetadata" ] []
        }


decodemeta allmeta =
    let
        all = D.dict (D.dict M.decodemetaval)
    in
    D.decodeString all allmeta


getformula baseurl =
    Http.get
        { expect =
              Http.expectString GotAllFormula
        , url =
            UB.crossOrigin baseurl
                [ "tssearch", "allformula" ] []
        }


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
    { model | filtered = List.sort model.catalog.series }


fragmentsmatcher query item =
    -- predicate for item containing all query space-separated parts
    List.all
        (\queryfragment -> String.contains queryfragment item)
        (String.split " " query)


namefilter model =
    case model.filterbyname of
        Nothing -> model
        Just match ->
            { model | filtered = List.filter (fragmentsmatcher match) model.filtered }


formulafilter model =
    case model.filterbyformula of
        Nothing -> model
        Just match ->
            let
                fragments = String.split " " match
                formula name =
                    Maybe.withDefault "" <| Dict.get name model.formula
                informula name =
                    -- formula part -> name
                    fragmentsmatcher match <| formula name
                series = List.filter informula model.filtered
            in
            { model | filtered = series }


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
    { model | filtered = catalogfilter
                         model.filtered
                         model.catalog.seriesBySource
                         model.selectedsources
    }


kindfilter model =
    { model | filtered = catalogfilter
                         model.filtered
                         model.catalog.seriesByKind
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
                case Dict.get name model.metadata of
                    Nothing -> False
                    Just meta ->
                        List.all (match meta) model.filterbymeta
        in
        { model | filtered = List.filter bymeta model.filtered }


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
                newmodel = { model
                               | catalog = cat
                               , filtered = cat.series
                               , selectedkinds = Dict.keys cat.seriesByKind
                               , selectedsources = Dict.keys cat.seriesBySource
                           }
            in
            if List.isEmpty newmodel.catalog.series then
                U.nocmd newmodel
            else
                ( newmodel
                , Cmd.batch
                    [ getmeta model.baseurl
                    , getformula model.baseurl
                    ]
                )

        GotMeta (Ok rawmeta) ->
            case decodemeta rawmeta of
                Ok meta ->
                    U.nocmd { model | metadata = meta }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotMeta (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        GotAllFormula (Ok rawformulae) ->
            case decodeformulae rawformulae of
                Ok formulae ->
                    U.nocmd { model | formula = formulae }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotAllFormula (Err err) ->
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


viewnamefilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter by name"
                , onInput NameFilter
                ] []
    in
    H.map (provideInput >> DebounceNameFilter) input


viewformulafilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter on formula content"
                , onInput FormulaFilter
                ] []
    in
    H.map (provideInput >> DebounceFormulaFilter) input


viewkindfilter model =
    let
        kinds = Dict.keys model.catalog.seriesByKind
        checkbox kind =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series type"]
                [ H.input
                    [ A.attribute "type" "checkbox"
                    , A.class "form-check-input"
                    , A.value kind
                    , A.checked <| List.member kind model.selectedkinds
                    , onClick <| KindUpdated kind
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
        sources = Dict.keys model.catalog.seriesBySource
        checkbox source =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series source"]
                [ H.input
                      [ A.attribute "type" "checkbox"
                      , A.class "form-check-input"
                      , A.value source
                      , A.checked <| List.member source model.selectedsources
                      , onClick <| SourceUpdated source
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
                , onClick AddMetaItem
                ]
                [ H.text "add" ]
        delete key value =
            H.button
                [ A.attribute "type" "button"
                , A.title "remove the metadata filter rule"
                , A.class "btn btn-warning btn-sm"
                , onClick (MetaItemToDelete (key, value))
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
               (Just <| onInput NewKey)
               (Just <| onInput NewValue)
         ] ++ List.map
             (\x -> fields (U.first x) (U.snd x) (Just delete) Nothing Nothing)
             model.filterbymeta
        )


viewfilteredqty model =
    let
        len = List.length model.filtered
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
            findkeysofvalue [] catalog.seriesByKind (Dict.keys catalog.seriesByKind) name True


seriessources name catalog =
    findkeysofvalue [] catalog.seriesBySource (Dict.keys catalog.seriesBySource) name False


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
                           [ A.style "float" "inline-end" ]
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
        slicelist inlist outlist count =
            case inlist of
                [] -> outlist
                head::tail ->
                    if count == 0
                    then outlist
                    else slicelist tail (head::outlist) (count - 1)

        filteredslice =
            slicelist filtered [] 1000

        missing =
            (List.length filteredslice) < (List.length filtered)

        items =
            List.map item <| List.sort filteredslice

        noseries = (List.length filtered) == 0

        tailnode =
            if missing || noseries  then
                ("_sliced", H.span [] [ H.text "" ])
            else
                ("_unsliced", H.span [] [ H.text "(full search results shown)" ])
    in
    K.node "ul"
        [ A.class "list-group list-group-flush" ]
        <| items ++ [ tailnode ]


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
        nbsources = Dict.size model.catalog.seriesBySource
    in
    H.div [ A.style "margin" ".5em" ]
        [ H.h1 [] [ H.text "Series Catalog" ]
        , H.div
              [ A.class "tsview-form-input small" ]
              [ viewnamefilter
              , H.span [] [ H.text " " ]
              , viewformulafilter
              , viewmetafilter model
              , viewkindfilter model
              , viewsourcefilter model
              , viewfilteredqty model
              ]
        , L.lazy5 viewfiltered
            model.baseurl model.filtered model.catalog (nbsources > 1) model.selectedsources
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
           init input =
               ( Model
                     input.baseurl
                     Cat.empty
                     Dict.empty
                     Dict.empty
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
               ,
                   Cmd.map GotCatalog <| Cat.get input.baseurl 1
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
