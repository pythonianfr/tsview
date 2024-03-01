module Queryeditor exposing ( main )

import Browser
import Html as H


type alias Model =
    { baseurl : String }


type Msg =
    Noop


view model =
    H.div [] []


update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


type alias Input =
    { baseurl : String }


main : Program Input Model Msg
main =
       let
           init input =
               let
                   model =
                       { baseurl = input.baseurl }
               in
               ( model, Cmd.none )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }

