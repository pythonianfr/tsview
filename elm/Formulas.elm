module Formulas exposing (main)
import Browser
import Html as H


type alias Model =
    { baseurl : String
    , name : String
    }

type Msg =
    InitMsg


view : Model -> H.Html Msg
view model =
    H.div [ ][ H.text "hello world !"]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitMsg ->
            (model, Cmd.none)


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input Model Msg
main =
    let
        init input =
            let
                model =
                    { baseurl = input.baseurl
                    , name = input.name
                    }
            in ( model , Cmd.none )

    in Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none

    }