module Search exposing (main)

import Browser
import Catalog as Cat
import Dict exposing (Dict)
import Html exposing (..)
import Url.Builder as UB


type alias Model =
    { baseurl : String
    , catalog : Cat.Model
    }


type Msg
    = Ok


nocmd model = ( model, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    nocmd model


view : Model -> Html Msg
view model =
    div []
        [ text "Series Catalog" ]


type alias Input =
    { baseurl : String }


main : Program Input  Model Msg
main =
       let
           init input =
               ( Model
                     input.baseurl
                     <| Cat.Model
                         []
                         Dict.empty
                         Dict.empty
                         []
               ,
                   Cmd.none
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
