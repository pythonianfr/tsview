module Info exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Url.Builder as UB


type Meta
    = String
    | Int
    | Bool


type alias Model =
    { baseurl : String
    , name : String
    , metadata : Dict String Meta
    }

type Msg
    = Ok


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [text ("Series " ++ model.name)]


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input  Model Msg
main =
       let
           init input =
               ( Model
                     input.baseurl
                     input.name
                     Dict.empty
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
