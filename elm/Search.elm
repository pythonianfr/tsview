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
    = GotCatalog Cat.Msg


nocmd model = ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog catmsg ->
            nocmd { model | catalog = Cat.update catmsg model.catalog }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Series Catalog" ]
        , p [] [ text (String.fromInt (List.length model.catalog.series) ++ " items") ]
        ]


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
                   Cmd.map GotCatalog <| Cat.get input.baseurl 1
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
