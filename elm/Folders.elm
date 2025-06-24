port module Folders exposing (main)

import Browser
import Json.Decode as JD
import Json.Encode as JE
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
import Task
import Tree exposing
    ( Tree
    , tree
    )
import Url.Builder as UB

import FoldersUtil exposing
    ( Cut(..)
    , Payload
    , decodeTree
    , buildTree
    , emptyTree
    , getPayload
    , mutePayload
    , pasteSeries
    , root
    , viewTree
    )

import FoldersUtil exposing (MsgTree(..))

import Util as U

port copySignal: (Bool -> msg) -> Sub msg
port pasteSignal: (Bool -> msg) -> Sub msg


type alias Model =
    { baseUrl: String
    , treeAttribute: Maybe String
    , paths: List String
    , tree: Tree Payload
    , currentDrag: Maybe ( String, Path )
    , overDrag: Maybe Path
    , errors: List String
    , currentCut: Cut
    , focus : Maybe Path
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | GotTreeAttribute ( Result Http.Error String )
    | GotSeries Path ( Result Http.Error String )
    | GotUpdatePath Path Path ( Result Http.Error String )
    | FromTree MsgTree
    | CopyFromBrowser Bool
    | PasteFromBrowser Bool


-- for documentation purpose
type alias Path = String

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPaths (Ok raw ) ->
            case JD.decodeString decodeTree raw of
                ( Ok paths ) ->
                    ( { model | paths = paths
                              , tree =
                                mutePayload
                                    ( "." ++ root )
                                    (\ p -> {p | open = True})
                                        <| buildTree paths
                      }
                    , Cmd.none )
                ( Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }
        GotPaths ( Err err ) ->
            U.nocmd model

        GotTreeAttribute (Ok raw) ->
            case JD.decodeString JD.string raw of
                ( Ok treeAttribute ) ->
                    U.nocmd { model | treeAttribute = Just treeAttribute }
                ( Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }
        GotTreeAttribute ( Err err ) ->
            U.nocmd model

        GotSeries path (Ok raw) ->
            case JD.decodeString ( JD.list JD.string ) raw of
                (Ok seriesL ) ->
                    U.nocmd
                        { model | tree
                            = mutePayload
                                path
                                (\p -> { p | series = Set.fromList seriesL
                                       }
                               )
                                model.tree
                        }
                (Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }

        GotSeries path  ( Err err ) ->
            U.nocmd model

        GotUpdatePath source destination (Ok _) ->
            ( model
            , Cmd.batch [ Task.perform
                            identity
                            ( Task.succeed ( FromTree ( Open source True )))
                        , Task.perform
                            identity
                            ( Task.succeed ( FromTree ( Open destination True )))
                        ]
            )


        GotUpdatePath source destination (Err _ ) ->
            U.nocmd model


        CopyFromBrowser _ ->
            case model.focus of
                Nothing -> U.nocmd model
                Just focus ->
                    let cut = (getPayload focus model.tree).selected
                    in
                    U.nocmd { model | currentCut = Cut focus cut }


        PasteFromBrowser _ ->
            case model.focus of
                Nothing -> U.nocmd model
                Just focus ->
                    case model.currentCut of
                        NoCut -> U.nocmd model
                        Cut from series ->
                            ({ model | tree =
                                        pasteSeries
                                            from
                                            focus
                                            series
                                            model.tree
                                    , currentCut = NoCut
                             }
                            , updatePath
                                model.baseUrl
                                model.treeAttribute
                                series
                                from
                                focus
                            )


        FromTree msgTree ->
            case msgTree of
                Open path open ->
                    ( { model | tree = mutePayload
                                        path
                                        (\ p -> {p | open = open})
                                        model.tree
                      }
                    , if not open
                        then Cmd.none
                        else
                            getSeries
                                model.baseUrl
                                path

                    )
                DragStart name from ->
                    ( { model | currentDrag = Just (name, from)}
                    , Cmd.none
                    )
                DragOver over ->
                     ( { model | overDrag = Just over
                     }
                    , Cmd.none
                    )
                DragEnd ->
                    ( { model | currentDrag = Nothing
                              , overDrag = Nothing
                      }
                    , Cmd.none
                    )
                Drop destination ->
                    case model.currentDrag of
                        Nothing -> U.nocmd model
                        Just (series, source) ->
                            ( moveSeries
                                { model | overDrag = Nothing }
                                destination
                            , updatePath
                                model.baseUrl
                                model.treeAttribute
                                ( Set.singleton series)
                                source
                                destination
                            )
                Select path name ->
                    U.nocmd
                        { model |
                            tree =
                             mutePayload
                                path
                                (\p -> { p | selected =
                                                Set.insert
                                                    name
                                                    p.selected
                                       }
                               )
                                model.tree
                        }
                Deselect path name ->
                    U.nocmd
                        { model |
                            tree =
                             mutePayload
                                path
                                (\p -> { p | selected =
                                                Set.remove
                                                    name
                                                    p.selected
                                       }
                               )
                                model.tree
                        }
                Focus path ->
                    U.nocmd { model | focus = Just path }


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


getTreeAttribute: String -> Cmd Msg
getTreeAttribute baseUrl =
    Http.get
        { url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-attribute"]
                [ ]
        , expect =  Http.expectString GotTreeAttribute
        }


updatePath: String -> Maybe String -> Set String -> Path -> Path -> Cmd Msg
updatePath baseUrl treeAttribute series source destination =
    case treeAttribute of
        Nothing -> Cmd.none
        Just treeA ->
            -- the metadata dict must be send as string
            let newMetadata
                    = JE.string
                        <| JE.encode 0 -- value to string
                            <| JE.object
                                [( treeA, JE.string destination )]
            in
            Cmd.batch
            <| List.map
                (\ name ->
                    Http.request
                    { method = "PATCH"
                    , url =
                        UB.crossOrigin
                            baseUrl
                            ["api", "series", "metadata"]
                            []
                    , body = Http.jsonBody
                                <| JE.object
                                    [( "name", JE.string name )
                                    ,( "metadata", newMetadata)
                                    ]
                    , expect = Http.expectString
                                ( GotUpdatePath source destination )
                    , headers = [ ]
                    , tracker = Nothing
                    , timeout = Nothing
                    }
                )
                ( Set.toList series )


getSeries: String -> String -> Cmd Msg
getSeries baseUrl path =
    Http.get
        { url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-path"]
                [ UB.string "name" path
                , UB.string "type" "pathname"
                ]
        , expect = Http.expectString (GotSeries path)
        }


moveSeries: Model -> Path -> Model
moveSeries model destination =
    case model.currentDrag of
        Nothing -> model
        Just (name, source) ->
            if source == destination
            then model
            else
            let newTree =
                    mutePayload
                        source
                        (\ p -> { p | series = Set.remove
                                                name
                                                p.series
                                }
                        )
                        <| mutePayload
                            destination
                            (\ p -> { p | series = Set.insert
                                                    name
                                                    p.series
                                    }
                            )
                            model.tree
            in { model | tree = newTree }


view: Model -> Html Msg
view model =
    case model.treeAttribute of
        Nothing -> div [] [text "No tree attribute defined"]
        Just _ ->
            div
                [ class "menu-folders" ]
                [ viewTree
                    model.tree
                    model.overDrag
                    FromTree
                    model.focus
                    model.currentCut
                , div
                    []
                    [text <| case model.currentDrag of
                                Nothing -> "no-drag"
                                Just ( name, path) -> name ++ path
                    ]
                , div [] [text <|  "over : "
                            ++ (case model.overDrag of
                                    Nothing -> "no-over"
                                    Just over -> over
                                )
                          ]
                ]



initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , treeAttribute = Nothing
    , paths = []
    , tree = emptyTree
    , currentDrag = Nothing
    , overDrag = Nothing
    , errors = []
    , currentCut = NoCut
    , focus = Nothing
    }


sub: Model -> Sub Msg
sub model = Sub.batch
            [ copySignal CopyFromBrowser
            , pasteSignal PasteFromBrowser
            ]

type alias Input =
    { baseurl : String }

init: Input -> ( Model, ( Cmd Msg ))
init input =
    ( initModel input.baseurl
    , Cmd.batch [ getPaths input.baseurl
                , getTreeAttribute input.baseurl
                ]
   )

main = Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }