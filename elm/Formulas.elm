module Formulas exposing (main)
import Browser
import Http
import Html as H
import Url.Builder as UB


type alias Model =
    { baseurl : String
    , name : String
    }


type Msg =
    GotFormulas (Result Http.Error String)


view : Model -> H.Html Msg
view model =
    H.div [ ][ H.text "hello world !"]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFormulas (Ok listFormulas) ->
            ( model, Cmd.none )
        GotFormulas (Err err) ->
            ( model, Cmd.none )


getSeriesInfo : Model -> Cmd Msg
getSeriesInfo  model =
    Http.get
        { expect = Http.expectString GotFormulas
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "find" ]
                [ UB.string "query" "(by.formula)"
                , UB.string "meta" "true"]
        }


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
            in ( model , getSeriesInfo model )

    in Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none

    }