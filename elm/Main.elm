module Main exposing (main)

import Browser
import Dict
import Html.Styled exposing (..)
import Http
import Json.Decode as Decode


type alias Model =
    { series : List String
    , status : Maybe String
    }


type alias SeriesCatalog =
    Dict.Dict String String


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)


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
    ul [] <| List.map (\x -> li [] [ text x ]) model.series


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
