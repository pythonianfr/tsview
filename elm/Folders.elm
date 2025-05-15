module Folders exposing (main)

import Browser
import Html exposing
    ( Html
    , Attribute
    , div
    , text
    )

type alias Model =
    { baseUrl: String }

type Msg =
    Something

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl }


view: Model -> Html Msg
view model =
    div [] [text "Hello world"]


sub: Model -> Sub Msg
sub model = Sub.none

type alias Input =
    { baseurl : String }

init: Input -> ( Model, ( Cmd Msg ))
init input =
    ( initModel input.baseurl
    , Cmd.none
   )

main = Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }