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
import Set exposing (Set)
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
    , fillPath
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
    , currentDrag: Maybe ( String, Int )
    , overDrag: Maybe Int
    , errors: List String
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | GotSeries Int String ( Result Http.Error String )
    | Open Int Bool
    | Drag String Int
    | DragHover Int
    | DragStop
    | Drop Int


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
                                        <| fillPath ""
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

        GotSeries idx path (Ok raw) ->
            -- integrity check (ie the tree is correctly indexed)
            if ( getPayload idx model.tree ).path /= path
            then U.nocmd model
            else
            case JD.decodeString ( JD.list JD.string ) raw of
                (Ok seriesL ) ->
                    U.nocmd
                        { model | tree
                            = mutePayload
                                idx
                                (\p -> { p | series = Set.fromList seriesL })
                                model.tree
                        }
                (Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }

        GotSeries idx path  ( Err err ) ->
            U.nocmd model
        Open idx open ->
            ( { model | tree = mutePayload
                                idx
                                (\ p -> {p | open = open})
                                model.tree
              }
            , if not open
                then Cmd.none
                else
                    let path = (getPayload idx model.tree).path
                    in
                        getSeries
                            model.baseUrl
                            idx
                            path

            )
        Drag name from ->
            ( { model | currentDrag = Just (name, from)}
            , Cmd.none
            )
        DragHover over ->
             ( { model | overDrag = Just over }
            , Cmd.none
            )
        DragStop ->
            ( { model | currentDrag = Nothing }
            , Cmd.none
            )
        Drop position ->
            ( moveSeries model position
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


getSeries: String -> Int -> String -> Cmd Msg
getSeries baseUrl idx path =
    Http.get
        { url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-path"]
                [ UB.string "name" path
                , UB.string "type" "pathname"
                ]
        , expect = Http.expectString (GotSeries idx path)
        }


moveSeries: Model -> Int -> Model
moveSeries model destination =
    case model.currentDrag of
        Nothing -> model
        Just (name, source) ->
            let newTree =
                    mutePayload
                        source
                        (\ p -> { p | series = Set.remove name p.series } )
                        <| mutePayload
                            destination
                            (\ p -> { p | series = Set.insert name p.series } )
                            model.tree
            in { model | tree = newTree }


view: Model -> Html Msg
view model =
    div
        [ class "menu-folders" ]
        [ viewTree
            model.tree
            Open
            Drag
            DragHover
            DragStop
            Drop

        , div
            []
            [text <| case model.currentDrag of
                        Nothing -> "no-drag"
                        Just ( name, position) -> name ++ String.fromInt position
            ]
        , div [] [text <|  "over : "
                    ++ (case model.overDrag of
                            Nothing -> "no-over"
                            Just over -> String.fromInt over
                        )
                  ]
        ]



initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , paths = []
    , tree = emptyTree
    , currentDrag = Nothing
    , overDrag = Nothing
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