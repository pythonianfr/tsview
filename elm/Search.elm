module Search exposing (main)

import Browser
import Catalog as Cat
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import Metadata as M
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    , catalog : Cat.Model
    , metadata : Dict String M.MetaVal
    , filtered : List String
    , errors : List String
    }


type Msg
    = GotCatalog Cat.Msg
    | GotMeta (Result Http.Error String)
    | NameFilter String


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
                cat = Cat.update catmsg model.catalog
                newmodel = { model
                               | catalog = cat
                               , filtered = cat.series
                           }
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

        NameFilter value ->
            U.nocmd { model | filtered = List.filter
                          (\x -> String.contains value x)
                          model.catalog.series
                    }


view : Model -> Html Msg
view model =
    let
        item elt =
            li [] [ text elt ]
    in
    div []
        [ h1 [] [ text "Series Catalog" ]
        , input
              [ A.class "form-control"
              , A.placeholder "filter by name"
              , onInput NameFilter
              ] []
        , ul []
            <| List.map item <| List.sort model.filtered
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
