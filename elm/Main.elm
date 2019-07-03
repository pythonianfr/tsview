module Main exposing (main)

import Browser
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Tachyons.Classes as T


type alias Model =
    { series : List String
    , status : Maybe String
    }


type alias SeriesCatalog =
    Dict.Dict String String


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)


classes : List String -> Attribute msg
classes xs =
    class (String.join " " xs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel x =
            ( x, Cmd.none )
    in
    case msg of
        CatalogReceived (Ok x) ->
            newModel { model | series = Dict.keys x }

        CatalogReceived (Err _) ->
            newModel { model | status = Just "Error on CatalogReceived" }


view : Model -> Html Msg
view model =
    let
        ul_class =
            classes [ T.list, T.pl0, T.ml0, T.center, T.mw5, T.ba, T.b__light_silver, T.br3 ]

        li_class =
            classes [ T.ph3, T.pv2, T.bb, T.b__light_silver ]

        renderSeries xs =
            ul [ ul_class ] <| List.map (\x -> li [ li_class ] [ text x ]) xs

        article_class =
            classes [ T.mw5, T.mw6_ns, T.center, T.pt4 ]

        div_class =
            classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ]
    in
    article [ article_class ] [ div [ div_class ] [ renderSeries model.series ] ]


main : Program () Model Msg
main =
    let
        initialGet =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url = "http://tshistory.test.pythonian.fr/series/catalog"
                }

        init _ =
            ( Model [] Nothing, initialGet )
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
