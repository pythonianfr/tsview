module Folders exposing (main)

import Browser
import Json.Decode as JD
import Html exposing
    ( Html
    , Attribute
    , br
    , div
    , li
    , text
    , ul
    )
import Html.Attributes exposing
    ( class )
import Http
import Tree exposing
    ( Tree
    , tree
    )
import Url.Builder as UB

import FoldersUtil exposing
    ( Payload
    , decodeTree
    , buildTree
    , emptyTree
    , fillPostion
    , getPayload
    , mutePayload
    , viewTree
    )
import Util as U

type alias Model =
    { baseUrl: String
    , paths: List String
    , tree: Tree Payload
    , errors: List String
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | Open Int


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPaths (Ok raw ) ->
            case JD.decodeString decodeTree raw of
                ( Ok paths ) ->
                    ( { model | paths = paths
                              , tree =
                                mutePayload
                                    0
                                    (\ p -> {p | open = True})
                                        <| fillPostion
                                            <| buildTree paths
                      }
                    , Cmd.none )
                ( Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }
        GotPaths ( Err err ) ->
            U.nocmd model

        Open idx ->
            ( { model | tree = mutePayload
                                idx
                                (\ p -> {p | open = not p.open})
                                model.tree
              }
            , Cmd.none
            )


getPaths: String -> Cmd Msg
getPaths baseUrl =
    Http.get
        { url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree"]
                []
        , expect = Http.expectString GotPaths
        }


view: Model -> Html Msg
view model =
    div
        [ class "menu-folders" ]
        [ viewTree model.tree Open ]



initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , paths = []
    , tree = emptyTree
    , errors = []
    }


sub: Model -> Sub Msg
sub model = Sub.none

type alias Input =
    { baseurl : String }

init: Input -> ( Model, ( Cmd Msg ))
init input =
    ( initModel input.baseurl
    , getPaths input.baseurl
   )

main = Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }