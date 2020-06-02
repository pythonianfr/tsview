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


namefilter model =
    case model.filterbyname of
        Nothing -> model
        Just item ->
            { model | filtered = List.filter (String.contains item) model.filtered }


formulafilter model =
    case model.filterbyformula of
        Nothing -> model
        Just item ->
            let
                formula name =
                    Maybe.withDefault "" <| Dict.get name model.formula
                informula name =
                    -- formula part -> name
                    String.contains item <| formula name
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


metafilter model =
    if List.length model.filterbymeta == 0 then model else
        let
            match meta query =
                case query of
                    (key, "") -> Dict.member key meta
                    ("", value) -> List.member value
                                   <| List.map M.metavaltostring
                                   <|Dict.values meta
                    (key, value) -> case Dict.get key meta of
                                        Nothing -> False
                                        Just metavalue ->
                                            value == M.metavaltostring metavalue
            bymeta name =
                case Dict.get name model.metadata of
                    Nothing -> False
                    Just meta ->
                        List.any (match meta) model.filterbymeta
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
                [ A.class "form-control"
                , A.placeholder "filter by name"
                , onInput NameFilter
                ] []
    in
    H.map (provideInput >> DebounceNameFilter) input


viewformulafilter =
    let input =
            H.input
                [ A.class "form-control"
                , A.placeholder "filter on formula content"
                , onInput FormulaFilter
                ] []
    in
    H.map (provideInput >> DebounceFormulaFilter) input


viewkindfilter model =
    let
        kinds = Dict.keys model.catalog.seriesByKind
        checkbox kind =
            H.div [ A.class "form-check form-check-inline" ]
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
    H.div [] (List.map checkbox kinds)


viewsourcefilter model =
    let
        sources = Dict.keys model.catalog.seriesBySource
        checkbox source =
            H.div
                [ A.class "form-check form-check-inline" ]
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
    H.div [] (List.map checkbox sources)


viewmetafilter model =
    let
        addentry =
            H.button
                [ A.attribute "type" "button"
                , A.class "btn btn-primary"
                , onClick AddMetaItem
                ]
                [ H.text "add" ]
        delete key value =
            H.button
                [ A.attribute "type" "button"
                , A.class "btn btn-warning"
                , onClick (MetaItemToDelete (key, value))
                ]
                [ H.text "delete" ]
        fields k v deleteaction keycb valuecb =
            H.div
                [ A.class "form-row" ]
                (List.append
                [ H.div
                      [ A.class "col-3" ]
                      [ H.input
                            ([ A.attribute "type" "text"
                             , A.class "form-control"
                             , A.placeholder "key"
                             , A.value k
                             ] ++ case keycb of
                                      Nothing -> []
                                      Just cb -> [ cb ]
                            ) []
                      ]
                , H.div [ A.class "col-6" ]
                    [ H.input
                          ([ A.attribute "type" "text"
                           , A.class "form-control"
                           , A.placeholder "value"
                           , A.value v
                           ] ++ case valuecb of
                                    Nothing -> []
                                    Just cb -> [ cb ]
                          ) []
                    ]
                ] [
                 case deleteaction of
                     Nothing -> addentry
                     Just action -> action k v
                ])
    in
    H.form
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
    H.p
        []
        [ H.text ("Found "
                      ++ String.fromInt (List.length model.filtered)
                      ++ " items.")
        ]


viewfiltered baseurl filtered =
    let
        item elt =
            (elt, H.li
                 [ A.class "list-group-item" ]
                 [ H.a [ A.href (UB.crossOrigin
                                     baseurl
                                     [ "tsinfo" ]
                                     [ UB.string "name" elt ]
                                )
                       ] [ H.text elt ]
                 ])
    in
    K.node "ul"
        [ A.class "list-group list-group-flush" ]
        <| List.map item <| List.sort filtered


viewerrors model =
    if List.length model.errors > 0 then
    H.div []
        [ H.h2 [] [ H.text "Errors" ]
        , H.div [] (List.map (\x -> H.p [] [ H.text x ]) model.errors)
        ]
    else H.span [] []


view : Model -> H.Html Msg
view model =
    H.div [ A.style "margin" ".5em" ]
        [ H.h1 [] [ H.text "Series Catalog" ]
        , H.div
              [ A.class "tsview-form-input" ]
              [ viewnamefilter
              , viewformulafilter
              , viewsourcefilter model
              , viewkindfilter model
              , viewmetafilter model
              , viewfilteredqty model
              ]
        , L.lazy2 viewfiltered model.baseurl model.filtered
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
