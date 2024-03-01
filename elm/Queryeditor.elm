module Queryeditor exposing ( main )

import Browser
import Html as H
import Http
import Json.Decode as JD
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    -- all errors
    , errors : List String
    -- baskets
    , baskets : List String
    }


type Msg =
    GotBasketNames (Result Http.Error String)


getbaskets model =
    Http.get
        { expect = Http.expectString GotBasketNames
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "baskets" ] [ ]
        }


view model =
    let
        showbasket name =
            H.span [] [ H.text <| name ++ " " ]

    in
    H.div []
        [ H.h1 [] [ H.text "Baskets" ]
        , H.div [] <| List.map showbasket model.baskets
        ]


update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GotBasketNames (Ok rawbaskets) ->
            case JD.decodeString (JD.list JD.string) rawbaskets of
                Ok baskets ->
                    U.nocmd { model | baskets = baskets }
                Err err ->
                    doerr "getbasketnames decode" <| JD.errorToString err

        GotBasketNames (Err err) ->
            doerr "getbasketnames http" <| U.unwraperror err


type alias Input =
    { baseurl : String }


main : Program Input Model Msg
main =
       let
           init input =
               let
                   model =
                       { baseurl = input.baseurl
                       , errors = []
                       , baskets = []
                       }
               in
               ( model, getbaskets model )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
