module Operators exposing (main)
import Browser
import Html as H
import Html.Attributes as A
import Html exposing
    ( Html
    , Attribute
    )
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder)
import List
import Util as U


type alias Model =
    { baseUrl: String
    , operators: List Item
    }


type alias Item =
    { name: String
    , doc: String
    , source: String
    }


specDecoder : Decoder ( List Item )
specDecoder =
    JD.list
        (JD.map3 Item
            ( JD.field "name" JD.string )
            ( JD.field "doc" JD.string )
            ( JD.field "source" JD.string ))


type Msg =
    GotSpec (Result Http.Error ( List Item ))


getSpec: String -> Cmd Msg
getSpec baseUrl =
     Http.get
        { url = baseUrl ++ "tsformula/spec-operators"
        , expect = Http.expectJson GotSpec specDecoder
        }


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSpec (Ok spec) ->
            U.nocmd  { model | operators = spec }

        GotSpec (Err error) ->
            U.nocmd model


initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , operators = []
    }


init: String -> ( Model, ( Cmd Msg ))
init baseUrl =
    ( initModel baseUrl
    , getSpec baseUrl
    )


buildSection: Item -> Html Msg
buildSection item =
    H.div
        [ ]
        [ H.h3
            [ A.class "first"]
            [ H.p
                [ A.class "name" ]
                [ H.text item.name ]
            , H.p
                [ A.class "from" ]
                [ H.text  " from : " ]
            , H.p
                [ A.class "source" ]
                [ H.text  item.source  ]
            ]
        , H.pre
            [ A.class "doc"]
            <| processDoc item.doc
        ]


processDoc: String -> List (Html Msg)
processDoc doc =
    List.map
        (\ line -> H.p [] [H.text line])
        ( String.split "\n" doc )


view: Model -> Html Msg
view model =
    H.div
        []
        [ H.h1 [ A.class "page-title" ] [ H.text "Operators documentation" ]
        , H.div
            [ A.class "operators"]
            (List.map buildSection model.operators)
        ]


sub model =  Sub.none


main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = sub
       }
