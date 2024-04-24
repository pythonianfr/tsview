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

import Menu as Men

type alias Model =
    { baseUrl: String
    , operators: List Item
    , menu : Men.Model
    }

type alias Item =
    { name: String
    , doc: String
    , source: String
    }


specDecoder : Decoder ( List Item )
specDecoder =
    JD.list (
         JD.map3 Item
           ( JD.field "name" JD.string )
           ( JD.field "doc" JD.string )
           ( JD.field "source" JD.string )
    )

type Msg =
    GotSpec (Result Http.Error ( List Item ))
    | Menu Men.Msg

getSpec: String -> Cmd Msg
getSpec baseUrl =
     Http.get
        { url = baseUrl ++ "tsformula/spec-operators"
        , expect = Http.expectJson GotSpec specDecoder
        }

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSpec (Ok spec)-> ( { model | operators = spec }
                            , Cmd.none )
        GotSpec (Err error)-> ( model, Cmd.none )
        Menu menumsg ->
            ( { model | menu = Men.updateModel menumsg model.menu }
            , Men.buildCmd menumsg model.menu)


initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , operators = []
    , menu = Men.initmenu "formula-documentation"
    }

init: String -> ( Model, ( Cmd Msg ))
init baseUrl =
    ( initModel baseUrl
    , Cmd.batch
        [ getSpec baseUrl
        , Men.getMenu baseUrl ( \ returnHttp ->  Menu (Men.GotMenu returnHttp ) )
        , Men.getIcons baseUrl ( \ returnHttp ->  Menu (Men.GotIcons returnHttp ) )
       ]
   )

buildSection: Item -> Html Msg
buildSection item =
    H.div
        []
        [ H.h3
            [A.class "first"]
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
            ( processDoc item.doc )]

processDoc: String -> List (Html Msg)
processDoc doc =
    List.map
        (\ line -> H.p [] [H.text line])
        ( String.split "\n" doc )

view: Model -> Html Msg
view model =
    H.div
        [ A.class ( if model.menu.menuModeText
                        then "grid-container-text"
                        else "grid-container-icon") ]
        [ Men.viewMenu model.menu Menu
        , H.div
            [ A.class "main-content operators"]
            (List.map buildSection model.operators)
        ]

sub model =  Men.loadMenuData (\ str -> Menu (Men.LoadMenuData str))

main = Browser.element
        { init = init
        , view = view
        , update = update
       , subscriptions = sub
        }