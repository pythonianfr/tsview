port module Folders exposing (main)

import Browser
import Dict exposing (Dict)
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
    , Drag(..)
    , Payload
    , SeriesAttribute
    , decodeTree
    , buildTree
    , emptyTree
    , getPayload
    , mutePayload
    , pasteSeries
    , root
    , selectFromQuery
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
    , currentDrag: Drag
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
                                (\p -> { p | series = dressSeries seriesL
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
                    let cut = Dict.keys
                                <| Dict.filter
                                    (\ k v -> v.selected)
                                    (getPayload focus model.tree).series
                    in
                    U.nocmd
                        <| { model | currentCut = Cut focus ( Set.fromList cut )
                                   , tree =
                                mutePayload
                                    focus
                                    (\ p -> { p | series =
                                                    Dict.map
                                                        (\ k v -> if v.selected
                                                                    then { v | cut = True }
                                                                    else v
                                                        )
                                                        p.series
                                            }
                                    )
                                    model.tree
                            }


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
                DragStart from name ->
                    ( { model | currentDrag = Drag from name}
                    , Cmd.none
                    )
                DragOver over ->
                     ( { model | overDrag = Just over
                     }
                    , Cmd.none
                    )
                DragEnd ->
                    ( { model | currentDrag = NoDrag
                              , overDrag = Nothing
                      }
                    , Cmd.none
                    )
                Drop destination ->
                    case model.currentDrag of
                        NoDrag -> U.nocmd model
                        Drag source series ->
                            ( moveSeries
                                { model | overDrag = Nothing }
                                source
                                destination
                                series
                            , updatePath
                                model.baseUrl
                                model.treeAttribute
                                series
                                source
                                destination
                            )
                Select path name ->
                    U.nocmd
                        { model |
                            tree =
                             mutePayload
                                path
                                (\p -> { p | series =
                                                Dict.map
                                                    (\ k v ->
                                                        if k == name
                                                        then
                                                        { v | selected = True }
                                                        else v
                                                    )
                                                    p.series
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
                                (\p -> { p | series =
                                                Dict.map
                                                    (\ k v ->
                                                        if k == name
                                                        then
                                                        { v | selected = False
                                                            , cut = False
                                                        }
                                                        else v
                                                    )
                                                    p.series
                                       }
                               )
                                model.tree
                        }
                Focus path ->
                    U.nocmd { model | focus = Just path }
                Query path query ->
                    U.nocmd { model | tree =
                                mutePayload
                                    path
                                    (\ p -> selectFromQuery { p | query = query} )
                                    model.tree
                            }


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


moveSeries: Model -> Path -> Path -> Set String ->Model
moveSeries model source destination names =
        if source == destination
        then model
        else
            { model | tree = moveSerieRec
                                ( Set.toList names )
                                source
                                destination
                                model.tree
            }


moveSerieRec : List String -> Path -> Path -> Tree Payload -> Tree Payload
moveSerieRec names source destination tree =
    case names of
        [] -> tree
        x :: xs ->
            moveSerieRec
                xs
                source
                destination
                ( moveSerie x source destination tree )


moveSerie: String -> Path -> Path -> Tree Payload -> Tree Payload
moveSerie name source destination tree =
    mutePayload
        source
        (\ p -> { p | series = Dict.remove
                                name
                                p.series
                }
        )
        <| mutePayload
            destination
            (\ p -> { p | series = Dict.insert
                                    name
                                    ( SeriesAttribute False False )
                                    p.series
                    }
            )
            tree


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
                ]



initModel: String -> Model
initModel baseUrl =
    { baseUrl = baseUrl
    , treeAttribute = Nothing
    , paths = []
    , tree = emptyTree
    , currentDrag = NoDrag
    , overDrag = Nothing
    , errors = []
    , currentCut = NoCut
    , focus = Nothing
    }


dressSeries : List String -> Dict String SeriesAttribute
dressSeries names =
    Dict.fromList
        <| List.map
            (\ name ->
                (name, { selected = False, cut = False })
            )
            names


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