module Plot exposing (main)

import Browser
import Common exposing (classes)
import Dict
import Either exposing (Either)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import KeywordMultiSelector
import KeywordSelector
import LruCache exposing (LruCache)
import Tachyons.Classes as T
import Task exposing (Task)
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , series : List String
    , searchString : String
    , searchedSeries : List String
    , selectedSeries : List String
    , selectedNamedSeries : List NamedSerie
    , activeSelection : Bool
    , cache : SeriesCache
    }


type alias SeriesCatalog =
    Dict.Dict String String


type alias Serie =
    Dict.Dict String Float


type alias NamedSerie =
    ( String, Serie )


type alias NamedError =
    ( String, String )


serieDecoder : Decoder Serie
serieDecoder =
    Decode.dict Decode.float


type alias SeriesCache =
    LruCache String Serie


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)
    | ToggleSelection
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | RenderPlot (Result (List String) ( SeriesCache, List NamedSerie, List NamedError ))


type alias Trace =
    { type_ : String
    , name : String
    , x : List String
    , y : List Float
    , mode : String
    }


encodeTrace : Trace -> Encode.Value
encodeTrace t =
    Encode.object
        [ ( "type", Encode.string t.type_ )
        , ( "name", Encode.string t.name )
        , ( "x", Encode.list Encode.string t.x )
        , ( "y", Encode.list Encode.float t.y )
        , ( "mode", Encode.string t.mode )
        ]


type alias TraceArgs =
    String -> List String -> List Float -> String -> Trace


scatterPlot : TraceArgs
scatterPlot =
    Trace "scatter"


type alias PlotArgs =
    { div : String
    , data : List Trace
    }


encodePlotArgs : PlotArgs -> Encode.Value
encodePlotArgs x =
    Encode.object
        [ ( "div", Encode.string x.div )
        , ( "data", Encode.list encodeTrace x.data )
        ]


plotFigure : List (Attribute msg) -> List (Html msg) -> Html msg
plotFigure =
    node "plot-figure"


fetchSeries :
    List String
    -> Model
    -> Task (List String) ( SeriesCache, List NamedSerie, List NamedError )
fetchSeries selectedNames model =
    let
        ( usedCache, cachedSeries ) =
            List.foldr
                (\name ( cache, xs ) ->
                    let
                        ( newCache, maybeSerie ) =
                            LruCache.get name cache

                        x : Either String NamedSerie
                        x =
                            maybeSerie
                                |> Either.fromMaybe name
                                |> Either.map (Tuple.pair name)
                    in
                    ( newCache, x :: xs )
                )
                ( model.cache, [] )
                selectedNames

        missingNames =
            Either.lefts cachedSeries

        getSerie : String -> Task String Serie
        getSerie serieName =
            Http.task
                { method = "GET"
                , url =
                    UB.crossOrigin
                        model.urlPrefix
                        [ "api", "series", "state" ]
                        [ UB.string "name" serieName ]
                , headers = []
                , body = Http.emptyBody
                , timeout = Nothing
                , resolver =
                    Http.stringResolver <|
                        Common.decodeJsonMessage serieDecoder
                }

        getMissingSeries : Task (List String) (List (Either String Serie))
        getMissingSeries =
            Common.taskSequenceEither <| List.map getSerie missingNames

        getSeries : List NamedSerie -> List NamedSerie
        getSeries missing =
            let
                series =
                    List.append (Either.rights cachedSeries) missing
                        |> Dict.fromList
            in
            List.foldr
                (\a b -> Common.maybe b (\x -> ( a, x ) :: b) (Dict.get a series))
                []
                selectedNames

        updateCache : List NamedSerie -> SeriesCache
        updateCache missing =
            List.foldl
                (\( name, serie ) cache -> LruCache.insert name serie cache)
                usedCache
                missing
    in
    getMissingSeries
        |> Task.onError (List.map Either.Left >> Task.succeed)
        |> Task.andThen
            (\missings ->
                let
                    xs : List (Either NamedError NamedSerie)
                    xs =
                        List.map2
                            (\name x ->
                                let
                                    addName =
                                        Tuple.pair name
                                in
                                Either.mapBoth addName addName x
                            )
                            missingNames
                            missings

                    missingSeries =
                        Either.rights xs
                in
                Task.succeed
                    ( updateCache missingSeries
                    , getSeries missingSeries
                    , Either.lefts xs
                    )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        removeItem x xs =
            List.filter ((/=) x) xs

        toggleItem x xs =
            if List.member x xs then
                removeItem x xs

            else
                x :: xs

        newModel x =
            ( x, Cmd.none )

        keywordMatch xm xs =
            if String.length xm < 2 then
                []

            else
                KeywordSelector.select xm xs |> List.take 20
    in
    case msg of
        CatalogReceived (Ok x) ->
            let
                series =
                    Dict.keys x
            in
            ( { model | series = series }
            , Task.attempt RenderPlot <| fetchSeries model.selectedSeries model
            )

        CatalogReceived (Err x) ->
            let
                _ =
                    Debug.log "Error on CatalogReceived" x
            in
            newModel model

        ToggleSelection ->
            newModel { model | activeSelection = not model.activeSelection }

        ToggleItem x ->
            let
                selectedSeries =
                    toggleItem x model.selectedSeries
            in
            ( { model | selectedSeries = selectedSeries }
            , Task.attempt RenderPlot <| fetchSeries selectedSeries model
            )

        SearchSeries x ->
            newModel { model | searchString = x }

        MakeSearch ->
            newModel { model | searchedSeries = keywordMatch model.searchString model.series }

        RenderPlot (Ok ( cache, namedSeries, namedErrors )) ->
            let
                _ =
                    Debug.log "Named errors" namedErrors
            in
            ( { model | cache = cache, selectedNamedSeries = namedSeries }, Cmd.none )

        RenderPlot (Err x) ->
            let
                _ =
                    Debug.log "Error on RenderPlot" x
            in
            newModel model


selectorConfig : KeywordMultiSelector.Config Msg
selectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            text
                "Type some keywords in input bar for selecting time series"
        , toggleMsg = ToggleItem
        }
    , actionSelector =
        { action = Nothing
        , defaultText = text ""
        , toggleMsg = ToggleItem
        }
    , onInputMsg = SearchSeries
    , divAttrs = [ classes [ T.mb4 ] ]
    }


view : Model -> Html Msg
view model =
    let
        plotDiv =
            "plotly_div"

        args =
            let
                data =
                    List.map
                        (\( name, serie ) ->
                            scatterPlot
                                name
                                (Dict.keys serie)
                                (Dict.values serie)
                                "lines"
                        )
                        model.selectedNamedSeries
            in
            PlotArgs plotDiv data |> encodePlotArgs |> Encode.encode 0

        selector =
            let
                cls =
                    classes [ T.pb2, T.f4, T.fw6, T.db, T.navy, T.link, T.dim ]

                children =
                    [ a [ cls, onClick ToggleSelection ] [ text "Series selection" ] ]

                ctx =
                    KeywordMultiSelector.Context
                        model.searchString
                        model.searchedSeries
                        model.selectedSeries
            in
            form [ classes [ T.center, T.pt4, T.w_90 ] ]
                (if model.activeSelection then
                    List.append children
                        [ KeywordMultiSelector.view selectorConfig ctx
                        ]

                 else
                    children
                )

        urls =
            let
                cls =
                    classes [ T.link, T.blue, T.lh_title ]

                permalink =
                    let
                        url =
                            UB.relative
                                [ "tsview" ]
                                (List.map
                                    (\x -> UB.string "series" x)
                                    model.selectedSeries
                                )
                    in
                    a [ A.href url, cls ] [ text "Permalink" ]

                histories =
                    List.map
                        (\x ->
                            a
                                [ A.href <| UB.relative [ "tshistory", x ] []
                                , A.target "_blank"
                                , cls
                                ]
                                [ text <| "View " ++ x ++ " history" ]
                        )
                        model.selectedSeries
            in
            ul [ classes [ T.list, T.mt3, T.mb0 ] ]
                (List.map
                    (\x -> li [ classes [ T.pv2 ] ] [ x ])
                    (permalink :: histories)
                )
    in
    div []
        [ header [ classes [ T.bg_light_blue ] ] [ selector ]
        , div [ A.id plotDiv ] []
        , plotFigure [ A.attribute "args" args ] []
        , footer [] [ urls ]
        ]


main : Program { urlPrefix : String, selectedSeries : List String } Model Msg
main =
    let
        initialGet urlPrefix =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url =
                    UB.crossOrigin urlPrefix
                        [ "api", "series", "catalog" ]
                        []
                }

        init flags =
            let
                p =
                    Common.checkUrlPrefix flags.urlPrefix

                c =
                    LruCache.empty 100

                s =
                    flags.selectedSeries
            in
            ( Model p [] "" [] s [] (List.isEmpty s) c
            , initialGet p
            )

        sub model =
            if model.activeSelection then
                Time.every 1000 (always MakeSearch)

            else
                Sub.none
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = sub
        }
