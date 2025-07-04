port module Folders exposing (main)

import Browser
import Browser.Events exposing
    ( onKeyDown
    )
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

import Metadata
import Util as U

import FoldersUtil exposing
    ( Cut(..)
    , Drag(..)
    , Path(..)
    , Payload
    , SeriesAttribute
    , decodeFind
    , decodeTree
    , dressSeries
    , buildTree
    , emptyTree
    , getOpenState
    , getPayload
    , mutePayload
    , pasteSeries
    , selectFromQuery
    , setOpenState
    , viewTree
    )

import FoldersUtil exposing (MsgTree(..))


port copySignal: (Bool -> msg) -> Sub msg
port pasteSignal: (Bool -> msg) -> Sub msg

keyDecoder: JD.Decoder Msg
keyDecoder =
    JD.map toKey ( JD.field "key" JD.string )

toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            Typing char
        _ ->
            ActionControl ( keyToType keyValue )


keyToType:  String -> ControlKey
keyToType keyValue =
    case keyValue of
        "Escape" -> Escape
        _ -> Other keyValue

type ControlKey
    = Escape
    | Other String

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
    , showUnclassified: Bool
    , creationName: String
    , openState: Set String
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | GotTreeAttribute ( Result Http.Error String )
    | GotSeries Path ( Result Http.Error String )
    | GotUpdatePath Path Path ( Result Http.Error String )
    | TowardDeletion Path Path String ( Result Http.Error String )
    | GotDelete Path ( Result Http.Error String )
    | FromTree MsgTree
    | CopyFromBrowser Bool
    | PasteFromBrowser Bool
    | ActionControl ControlKey
    | Typing Char


initTree: List String -> Tree Payload
initTree paths =
    mutePayload
        Root
        (\ p -> { p | open = True})
        ( buildTree paths )


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPaths (Ok raw ) ->
            case JD.decodeString decodeTree raw of
                ( Ok paths ) ->
                    ( { model | paths = paths
                              , tree = setOpenState
                                           ( initTree paths )
                                           model.openState
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
            let
             decoded
                = case path of
                    Root -> JD.decodeString decodeFind raw
                    Branch _ -> JD.decodeString ( JD.list JD.string ) raw
            in
            case decoded of
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

        TowardDeletion source destinaton name ( Ok raw ) ->
            let treeAttribute = Maybe.withDefault "" model.treeAttribute
            in
            case JD.decodeString Metadata.decodemeta raw of
                Ok meta ->
                    let newMeta =
                            Dict.remove treeAttribute meta
                    in
                        ( model
                        , replaceMetadata
                            model.baseUrl
                            name
                            newMeta
                            source
                            destinaton
                        )
                ( Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }

        TowardDeletion name _ _ ( Err raw ) -> U.nocmd model

        GotUpdatePath source destination (Err _ ) ->
            U.nocmd model

        GotDelete path (Ok _) ->
            ( model
            , getPaths model.baseUrl
            )

        GotDelete path (Err _) -> U.nocmd model

        CopyFromBrowser _ ->
             case model.focus of
                Nothing -> U.nocmd model
                Just focus ->
                    ( model
                    , Task.perform
                        identity
                        ( Task.succeed ( FromTree ( ButtonCut focus )))
                    )

        PasteFromBrowser _ ->
            case model.focus of
                Nothing -> U.nocmd model
                Just focus ->
                    ( model
                    , Task.perform
                        identity
                        ( Task.succeed ( FromTree ( ButtonPaste focus )))
                    )

        ActionControl ( key ) ->
            case key of
                Other _ -> U.nocmd model
                Escape ->
                    case model.focus of
                        Nothing -> U.nocmd model
                        Just focus ->
                            ( { model | focus = Nothing },
                            Task.perform
                                identity
                                ( Task.succeed ( FromTree ( ButtonReset focus ))))

        Typing _ -> U.nocmd model

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
                                                        { v | selected = False}
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

                ButtonCut path ->
                    let cut = Dict.keys
                                <| Dict.filter
                                    (\ k v -> v.selected)
                                    (getPayload path model.tree).series
                    in
                    U.nocmd
                        <| { model | currentCut = Cut path ( Set.fromList cut )                            }

                ButtonPaste path ->
                    case model.currentCut of
                        NoCut -> U.nocmd model
                        Cut from series ->
                            ({ model | tree =
                                        pasteSeries
                                            from
                                            path
                                            series
                                            model.tree
                                    , currentCut = NoCut
                             }
                            , updatePath
                                model.baseUrl
                                model.treeAttribute
                                series
                                from
                                path
                            )

                ButtonReset path ->
                    U.nocmd
                        { model
                            | tree =
                                mutePayload
                                    path
                                    (\ p ->
                                        { p | series =
                                                Dict.map
                                                    (\ k v ->
                                                        { v | selected = False }
                                                    )
                                                    p.series
                                            , query = ""
                                            }
                                    )
                                    model.tree
                            , currentCut = NoCut
                        }

                ShowRoot show ->
                    ( { model | showUnclassified
                                = not model.showUnclassified }
                    , if show
                        then getSeries
                                model.baseUrl
                                Root
                        else Cmd.none
                    )

                Delete path ->
                    ( { model | openState =
                                    getOpenState
                                        model.tree
                      }
                    , deletePath
                        model.baseUrl
                        path
                    )

                CreationName creationName ->
                    U.nocmd
                        { model | creationName =
                                    String.replace
                                        "."
                                        ""
                                        creationName
                        }

                Create path ->
                    if model.creationName == ""
                    then U.nocmd model
                    else
                    let prefix = case path of
                                    Root -> ""
                                    Branch p -> p ++ "."
                        newPath =  prefix ++  model.creationName
                        newPaths = model.paths
                                   ++
                                   [ newPath ]
                    in
                    ({ model | tree = setOpenState
                                        ( initTree newPaths )
                                        ( getOpenState model.tree)
                             , paths = newPaths
                             , creationName = ""
                      },
                      Task.perform
                            identity
                            ( Task.succeed ( FromTree ( Open path True )))
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


replaceMetadata: String -> String -> Metadata.Metadata -> Path -> Path -> Cmd Msg
replaceMetadata baseUrl name meta source destination =
    Http.request
        { method = "PUT"
        , url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "metadata"]
                []
        , body = Http.jsonBody
                    <| JE.object
                        [( "name", JE.string name )
                        ,( "metadata", JE.string
                                        <| Metadata.encodemeta meta )
                        ]
        , expect = Http.expectString
                    ( GotUpdatePath source destination )
        , headers = [ ]
        , tracker = Nothing
        , timeout = Nothing
        }


updatePath: String -> Maybe String -> Set String -> Path -> Path -> Cmd Msg
updatePath baseUrl treeAttribute series source destination =
    case treeAttribute of
        Nothing -> Cmd.none
        Just treeA ->
            case destination of
                Branch dest ->
                    -- the metadata dict must be send as string
                    let newMetadata
                            = JE.string
                                <| JE.encode 0 -- value to string
                                    <| JE.object
                                        [( treeA, JE.string dest )]
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
                Root ->
                    -- deletion of path attribute in the metadata
                    Cmd.batch
                    <| List.map
                        (\ name ->
                            Http.request
                            { method = "GET"
                            , url =
                                UB.crossOrigin
                                    baseUrl
                                    ["api", "series", "metadata"]
                                    [ UB.string "name" name ]
                            , body = Http.emptyBody
                            , expect = Http.expectString
                                        ( TowardDeletion source Root name )
                            , headers = [ ]
                            , tracker = Nothing
                            , timeout = Nothing
                            }
                        )
                        ( Set.toList series )


getSeries: String -> Path -> Cmd Msg
getSeries baseUrl path =
    case path of
        Branch pat ->
            Http.get
                { url =
                    UB.crossOrigin
                        baseUrl
                        ["api", "series", "tree-path"]
                        [ UB.string "name" pat
                        , UB.string "type" "pathname"
                        ]
                , expect = Http.expectString (GotSeries path)
                }
        Root ->
            let query = "(by.without-path)"
            in
            Http.get
                { url =
                    UB.crossOrigin
                        baseUrl
                        ["api", "series", "find"]
                        [ UB.string "query" query
                        ]
                , expect = Http.expectString (GotSeries path)
                }


deletePath: String -> Path -> Cmd Msg
deletePath baseUrl path =
    case path of
        Root -> Cmd.none
        Branch pat ->
            Http.request
                { method = "DELETE"
                , url =
                    UB.crossOrigin
                        baseUrl
                        ["api", "series", "tree-path"]
                        [ UB.string "path" pat
                        ]
                , expect = Http.expectString (GotDelete path)
                , body = Http.emptyBody
                , headers = [ ]
                , tracker = Nothing
                , timeout = Nothing
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
                                    ( SeriesAttribute False )
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
                    model.baseUrl
                    model.tree
                    model.overDrag
                    FromTree
                    model.focus
                    model.currentCut
                    model.showUnclassified
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
    , showUnclassified = False
    , creationName = ""
    , openState = Set.empty
    }



sub: Model -> Sub Msg
sub model = Sub.batch
            [ copySignal CopyFromBrowser
            , pasteSignal PasteFromBrowser
            , onKeyDown ( keyDecoder )
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