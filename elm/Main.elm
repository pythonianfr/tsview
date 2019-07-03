{- Compilation :

    $ elm make --output elm/elm.js elm/Main.elm

   Running :

    $ cd elm
    $ elm reactor&

-}


module Main exposing (main)

import Browser
import Dict
import Fuzzy exposing (match)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onInput, onMouseDown)
import Http
import Json.Decode as Decode
import Tachyons.Classes as T


type alias Model =
    { series : List String
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


classes : List String -> Attribute msg
classes xs =
    class (String.join " " xs)


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

        fuzzyMatch xm xs =
            if String.length xm < 2 then
                []

            else
                List.sortBy (\x -> match [] [] xm x |> .score) xs |> List.take 20
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
            newModel { model | searchedSeries = fuzzyMatch x model.series }


view : Model -> Html Msg
view model =
    let
        ul_class =
            classes [ T.list, T.pl0, T.ml0, T.center, T.mw5, T.ba, T.b__light_silver, T.br3 ]

        li_class =
            classes [ T.ph3, T.pv2, T.bb, T.b__light_silver, T.dim ]

        li_selected serie =
            let
                is_selected =
                    List.member serie model.selectedSeries
            in
            classList <| List.map (\x -> ( x, is_selected )) [ T.white, T.bg_blue ]

        li_attrs x =
            [ li_class, li_selected x, onMouseDown <| ToggleItem x ]

        renderSeries xs =
            ul [ ul_class ] <| List.map (\x -> li (li_attrs x) [ text x ]) xs

        article_class =
            classes [ T.mw5, T.mw6_ns, T.center, T.pt4 ]

        div_class =
            classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ]

        fuzzySelector =
            let
                input_class =
                    classes [ T.input_reset, T.ba, T.b__black_20, T.pa2, T.db, T.w_100 ]

                searchInput =
                    input [ input_class, onInput SearchSeries ] []

                cols =
                    List.map (\x -> div [ classes [ T.dtc, T.pa1 ] ] [ renderSeries x ])
                        [ model.searchedSeries, model.selectedSeries ]
            in
            [ div [ classes [ T.dt, T.dt__fixed ] ] [ searchInput ]
            , div [ classes [ T.dt, T.dt__fixed ] ] cols
            ]
    in
    article [ article_class ] [ div [ div_class ] fuzzySelector ]


main : Program () Model Msg
main =
    let
        initialGet =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url = "http://tshistory.test.pythonian.fr/series/catalog"
                }

        init _ =
            ( Model [] [] [] Nothing, initialGet )
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
