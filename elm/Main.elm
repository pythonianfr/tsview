{- Compilation :

    $ elm make --output elm/elm.js elm/Main.elm

   Running :

    $ cd elm
    $ elm reactor&

-}


module Main exposing (main)

import Browser
import Common exposing (classes)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onInput, onMouseDown)
import Http
import ItemSelector
import Json.Decode as Decode
import KeywordSelector
import Tachyons.Classes as T
import Time


type alias Model =
    { series : List String
    , searchString : String
    , searchedSeries : List String
    , selectedSeries : List String
    , status : Maybe String
    }


type alias SeriesCatalog =
    Dict.Dict String String


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)
    | ToggleItem String
    | SearchSeries String
    | MakeSearch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleItem x xs =
            if List.member x xs then
                List.filter ((/=) x) xs

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
            newModel { model | series = series }

        CatalogReceived (Err _) ->
            newModel { model | status = Just "Error on CatalogReceived" }

        ToggleItem x ->
            newModel { model | selectedSeries = toggleItem x model.selectedSeries }

        SearchSeries x ->
            newModel { model | searchString = x }

        MakeSearch ->
            newModel { model | searchedSeries = keywordMatch model.searchString model.series }


view : Model -> Html Msg
view model =
    let
        articleClass =
            classes [ T.center, T.pt4, T.w_90 ]

        divClass =
            classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ]

        fuzzySelector =
            let
                searchInput =
                    let
                        inputClass =
                            classes
                                [ T.input_reset
                                , T.dtc
                                , T.ba
                                , T.b__black_20
                                , T.pa2
                                , T.db
                                , T.w_100
                                ]
                    in
                    [ input [ inputClass, onInput SearchSeries ] [] ]

                cols =
                    let
                        attrs =
                            [ classes [ T.dtc, T.pa1 ] ]
                    in
                    List.map
                        (\x -> div attrs [ ItemSelector.view ToggleItem x model.selectedSeries ])
                        [ model.searchedSeries, model.selectedSeries ]
            in
            List.map
                (div [ classes [ T.dt, T.dt__fixed ] ])
                [ searchInput, cols ]
    in
    article [ articleClass ] [ div [ divClass ] fuzzySelector ]


main : Program () Model Msg
main =
    let
        initialGet =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url = "http://tshistory.test.pythonian.fr/series/catalog"
                }

        init _ =
            ( Model [] "" [] [] Nothing, initialGet )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = sub
        }
