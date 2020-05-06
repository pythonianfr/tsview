module Search exposing (main)

import Browser
import Catalog as Cat
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Html.Events exposing (onInput, onClick)
import Html.Keyed as K
import Http
import Json.Decode as D
import Metadata as M
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    , catalog : Cat.Model
    , metadata : Dict String M.MetaVal
    , filtered : List String
    , selectedkinds : List String
    , selectedsources : List String
    , errors : List String
    }


type Msg
    = GotCatalog Cat.Msg
    | GotMeta (Result Http.Error String)
    | NameFilter String
    | KindUpdated String
    | SourceUpdated String


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
        all = D.dict M.decodemetaval
    in
    D.decodeString all allmeta


insert list item =
    List.append list [item]


remove list item =
    List.filter ((/=) item) list


filter list item =
    List.filter (String.contains item) list


filterseries model catalog =
    -- filter by source and kinds
    -- NOTE: factor me with SeriesSelector.filterseries !
    let
        seriesbykind kind =
            Set.toList (Maybe.withDefault Set.empty (Dict.get kind catalog.seriesByKind))
        filteredseries =
            List.concat (List.map seriesbykind model.selectedkinds)
        filterbysource source =
            let
                series = Maybe.withDefault Set.empty (Dict.get source catalog.seriesBySource)
            in
                List.filter (\x -> Set.member x series) filteredseries
    in
        List.sort (List.concat (List.map filterbysource model.selectedsources))


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
                , getmeta model.baseurl
                )

        GotMeta (Ok rawmeta) ->
            case decodemeta rawmeta of
                Ok meta ->
                    U.nocmd { model | metadata = meta }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotMeta (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err

        NameFilter value ->
            let
                series = filterseries model model.catalog
            in
            U.nocmd { model | filtered = filter series value }

        KindUpdated kind ->
            let
                newkinds =
                    if List.member kind model.selectedkinds
                    then remove model.selectedkinds kind
                    else insert model.selectedkinds kind
                newmodel = { model | selectedkinds = newkinds }
                series = filterseries newmodel newmodel.catalog
            in
            U.nocmd { model
                        | selectedkinds = List.sort newkinds
                        , filtered = series
                    }

        SourceUpdated source ->
            let
                newsources =
                    if List.member source model.selectedsources
                    then remove model.selectedsources source
                    else insert model.selectedsources source
                newmodel = { model | selectedsources = newsources }
                series = filterseries newmodel newmodel.catalog
            in
            U.nocmd { model
                        | selectedsources = List.sort newsources
                        , filtered = series
                    }


viewnamefilter =
    H.input
    [ A.class "form-control"
    , A.placeholder "filter by name"
    , onInput NameFilter
    ] []


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


viewfiltered model =
    let
        item elt =
            (elt, H.li []
                 [ H.a [ A.href (UB.crossOrigin
                                     model.baseurl
                                     [ "tsinfo" ]
                                     [ UB.string "name" elt ]
                                )
                       ] [ H.text elt ]
                 ])
    in
    K.node "ul" [] <| List.map item <| List.sort model.filtered


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "Series Catalog" ]
        , viewnamefilter
        , viewsourcefilter model
        , viewkindfilter model
        , viewfiltered model
        ]


type alias Input =
    { baseurl : String }


main : Program Input  Model Msg
main =
       let
           init input =
               ( Model
                     input.baseurl
                     ( Cat.Model
                         []
                         Dict.empty
                         Dict.empty
                         []
                     )
                     Dict.empty
                     []
                     []
                     []
                     []
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
