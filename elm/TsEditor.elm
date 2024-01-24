module TsEditor exposing (main)

import Browser
import Html as H


-- MODEL


type alias Model =
    {}


init : Model
init =
    {}


-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)


-- VIEW


view : Model -> H.Html Msg
view model =
    H.div [] [ H.text "hello world" ]


-- SUBSCRIPTIONS


sub : Model -> Sub Msg
sub model =
    Sub.none


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( Model, Cmd.none )
        , update =  update
        , subscriptions = sub
        , view = view
        }