module Search exposing (main)

import Browser
import Catalog as Cat
import Dict exposing (Dict)
import Html exposing (..)
import Http
import Json.Decode as D
import Metadata as M
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    , catalog : Cat.Model
    , metadata : Dict String M.MetaVal
    , errors : List String
    }


type Msg
    = GotCatalog Cat.Msg
    | GotMeta (Result Http.Error String)


getmeta baseurl =
    Http.get
        { expect =
              Http.expectString GotMeta
        , url =
            UB.crossOrigin baseurl
                [ "tssearch", "allmetadata" ] []
        }


decodemeta allmeta =
    let
        all = D.dict M.decodemetaval
    in
    D.decodeString all allmeta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog catmsg ->
            let
                newmodel = { model | catalog = Cat.update catmsg model.catalog }
            in
            if List.isEmpty newmodel.catalog.series then
                U.nocmd newmodel
            else
                ( newmodel
                , getmeta model.baseurl
                )

        GotMeta (Ok rawmeta) ->
            case decodemeta rawmeta of
                Ok meta ->
                    U.nocmd { model | metadata = meta }
                Err err ->
                    U.nocmd <| U.adderror model <| D.errorToString err

        GotMeta (Err err) ->
            U.nocmd <| U.adderror model <| U.unwraperror err


view : Model -> Html Msg
view model =
    let
        item elt =
            li [] [ text elt ]
    in
    div []
        [ h1 [] [ text "Series Catalog" ]
        , ul []
            <| List.map item <| List.sort model.catalog.series
        ]


type alias Input =
    { baseurl : String }


main : Program Input  Model Msg
main =
       let
           init input =
               ( Model
                     input.baseurl
                     ( Cat.Model
                         []
                         Dict.empty
                         Dict.empty
                         []
                     )
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
