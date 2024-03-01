module Queryeditor exposing ( main )

import Browser
import Dict
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Lisp
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    -- all errors
    , errors : List String
    -- baskets
    , baskets : List String
    -- current basket
    , name : Maybe String
    , basket : Maybe String
    }


type Msg
    = GotBasketNames (Result Http.Error String)
    | SelectedBasket String
    | GotBasketDefinition (Result Http.Error String)


getbaskets model =
    Http.get
        { expect = Http.expectString GotBasketNames
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "baskets" ] [ ]
        }


getbasket model name =
    Http.get
        { expect = Http.expectString GotBasketDefinition
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "basket-definition" ]
                [ UB.string "name" name ]
        }


view model =
    let
        unpacksbasket basketname =
            JD.succeed <| SelectedBasket basketname

        basketoption basketname =
            H.option
                [ HA.value basketname ]
                [ H.text basketname ]

        basketslist =
            [ H.select
                  [ HA.name "basket"
                  , HE.on "change" (JD.andThen unpacksbasket HE.targetValue)
                  , HA.class "form-control"
                  ]
                  <| List.map basketoption model.baskets
            ]

        currentbasket =
            case model.basket of
                Just basket ->
                    case Lisp.parse basket of
                        Just tree -> H.div [] (Lisp.view tree Dict.empty )
                        Nothing -> H.span [] []

                Nothing ->
                    H.span [] []
    in
    H.div []
        [ H.h1 [] [ H.text "Baskets" ]
        , H.div [] basketslist
        , H.div [] [ currentbasket ]
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
                    ( { model | baskets = baskets }
                    , case List.head baskets of
                          Nothing -> Cmd.none
                          Just head -> getbasket model head
                    )

                Err err ->
                    doerr "getbasketnames decode" <| JD.errorToString err

        GotBasketNames (Err err) ->
            doerr "getbasketnames http" <| U.unwraperror err

        SelectedBasket name ->
            ( { model | name = Just name }
            , getbasket model name
            )

        GotBasketDefinition (Ok rawbasket) ->
            case JD.decodeString JD.string rawbasket of
                Ok def ->
                    U.nocmd { model | basket = Just def }

                Err err ->
                    doerr "getbasketdefinition decode" <| JD.errorToString err

        GotBasketDefinition (Err err) ->
            doerr "getbasketdefinition http" <| U.unwraperror err


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
                       , name = Nothing
                       , basket = Nothing
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
