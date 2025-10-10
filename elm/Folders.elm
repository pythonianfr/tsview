port module Folders exposing (main)

import Browser
import Browser.Events exposing
    ( onKeyDown
    )
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Html exposing
    ( Html
    , Attribute
    , div
    , input
    , p
    )
import Html.Attributes exposing
    ( class
    , title
    )
import Html.Events exposing
    ( onClick )
import Http
import Set exposing (Set)
import Task
import Tree exposing
    ( Tree )
import Url.Builder as UB

import FoldersUtil exposing
    ( Cut(..)
    , Drag(..)
    , MsgTree(..)
    , Path(..)
    , TypingType(..)
    , LoadingStatus(..)
    , Payload
    , SeriesAttribute
    , closeAllMenus
    , decodeFind
    , decodeTree
    , dressSeries
    , buildTree
    , emptyTree
    , getOpenState
    , getPayload
    , mutePayload
    , pathFromString
    , pasteSeries
    , renamePaths
    , reprPath
    , selectFromQuery
    , splitByBatch
    , viewTree
    )

import Horizon exposing
    ( PlotStatus(..)
    , HorizonModel
    , initHorizon
    , loadFromLocalStorage
    , selectHorizon
    , updateHorizon
    )
import Horizon as ModuleHorizon

import Metadata
import Util as U


port cutSignal: (Bool -> msg) -> Sub msg
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
        "Enter" -> Enter
        _ -> Other keyValue

type ControlKey
    = Escape
    | Enter
    | Other String


type alias Model =
    { baseUrl: String
    , paths: List String
    , tree: Tree Payload
    , restriction: Int
    , currentDrag: Drag
    , overDrag: Maybe Path
    , errors: List String
    , currentCut: Cut
    , focus : Maybe Path
    , currentTyping: TypingType
    , openState: Set String
    , currentTransactions : Dict (String, String) ( Dict Int ( List String ))
    , queryDebouncer: Debouncer Msg
    , horizon : HorizonModel
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | GotSeries Path ( Result Http.Error String )
    | GotAssignedPath Int Path Path String ( Result Http.Error String )
    | GotDelete Path ( Result Http.Error String )
    | GotRename String String ( Result Http.Error String )
    | ProcessTransaction Int ( Path, Path )
    | FromTree MsgTree
    | CopyFromBrowser Bool
    | PasteFromBrowser Bool
    | ActionControl ControlKey
    | Typing Char
    | DebounceQuery (Debouncer.Msg Msg)
    | Horizon ModuleHorizon.Msg


forbidden : List String
forbidden =
    [ "."
    , "+"
    , "/"
    , "-"
    , "("
    , ")"
    ]


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
                              , tree = initTree paths
                      }
                    , Cmd.batch
                        <| reOpen ( getOpenState model.tree )
                    )
                ( Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                            }
        GotPaths ( Err err ) ->
            U.nocmd model


        GotSeries path (Ok raw) ->
            let
             decoded
                = case path of
                    Unclassified -> JD.decodeString decodeFind raw
                    Branch _ -> JD.decodeString ( JD.list JD.string ) raw
                    Root -> JD.decodeString ( JD.list JD.string ) raw --should not occur
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
                                <| mutePayload
                                    path
                                    (\p -> { p | status = SuccessFolder })
                                    model.tree
                                    }
                (Err err ) ->
                    U.nocmd { model | errors = model.errors
                                               ++ [JD.errorToString err]
                                    , tree
                                        = mutePayload
                                            path
                                            (\p -> { p | status = DecodingErrorFolder })
                                            model.tree
                                        }

        GotSeries path  ( Err err ) ->
            U.nocmd
                { model | tree
                    = mutePayload
                        path
                        (\p -> { p | status = LoadingErrorFolder })
                        model.tree
                }

        GotAssignedPath idBatch source destination name _ ->
            let newModel = removeFromQueue idBatch source destination name model
                currentTransaction = Maybe.withDefault
                                        Dict.empty
                                        <| Dict.get
                                            (reprPath source, reprPath destination)
                                            newModel.currentTransactions
            in
                case Dict.get idBatch currentTransaction of
                    Nothing -> ( newModel
                               , Task.perform
                                    identity
                                    (Task.succeed
                                        <| ProcessTransaction
                                            ( idBatch + 1 )
                                            ( source, destination)
                                    )
                              )
                    Just _ -> U.nocmd newModel

        GotDelete path (Ok _) ->
            ( model
            , getPaths model.baseUrl
            )

        GotDelete path (Err _) -> U.nocmd model

        GotRename oldBranch newBranch (Ok _) ->
            ( model
            , getPaths model.baseUrl
            )

        GotRename oldBranch newBranch (Err _) -> U.nocmd model

        ProcessTransaction idBatch ( source, destination )
            -> let batchs = Dict.get
                                ( reprPath source, reprPath destination )
                                model.currentTransactions
               in
               case batchs of
                Nothing -> U.nocmd model -- should not occurred
                Just dictBatch ->
                    case Dict.get idBatch dictBatch of
                        Nothing -> -- end of transaction
                            let transaction = Dict.remove
                                                ( reprPath source, reprPath destination )
                                                model.currentTransactions
                            in
                            ( { model | currentTransactions = transaction }
                            , Cmd.batch
                                [ Task.perform
                                    identity
                                    ( Task.succeed ( FromTree ( Open source True False )))
                                , Task.perform
                                    identity
                                    ( Task.succeed ( FromTree ( Open destination True False )))
                                ]
                            )
                        Just listSeries ->
                            ( model
                            , Cmd.batch
                                <| List.map
                                    ( updateSinglePath
                                        model.baseUrl
                                        idBatch
                                        source
                                        destination
                                    )
                                    listSeries
                            )

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
                        Nothing -> U.nocmd { model | tree = closeAllMenus model.tree
                                                   , currentTyping = NoTyping
                                           }
                        Just focus ->
                            ( { model | focus = Nothing
                                      , tree = closeAllMenus model.tree
                                      , currentTyping = NoTyping
                              },
                            Task.perform
                                identity
                                ( Task.succeed ( FromTree ( ButtonReset focus ))))
                Enter ->
                    case model.currentTyping of
                        NoTyping -> U.nocmd model
                        Creating _ _ ->
                            ( model,
                             Task.perform
                                identity
                                ( Task.succeed ( FromTree Create ))
                            )
                        Renaming _ _ ->
                            ( model,
                             Task.perform
                                identity
                                ( Task.succeed ( FromTree Rename ))
                            )

        Typing _ -> U.nocmd model

        DebounceQuery val ->
            Debouncer.update update queryDebouncerConfig val model

        Horizon hMsg ->
            let ( newModelHorizon, commands ) =  updateHorizon
                                                    hMsg
                                                    Horizon
                                                    model.horizon
            in ( { model | horizon = newModelHorizon}, commands )


        FromTree msgTree ->
            case msgTree of
                Open path open focus ->
                    let newModel = { model | tree
                                        = mutePayload
                                            path
                                            (\ p -> {p | open = open})
                                            model.tree
                                   }
                        focusedModel = if open && focus
                                        then { newModel | focus = Just path }
                                        else newModel
                    in
                    if not open
                        then
                        ( newModel
                        , Cmd.none )
                        else
                        ( { focusedModel | tree =
                                mutePayload
                                    path
                                    (\ p -> { p | status = LoadingFolder })
                                    newModel.tree
                          }
                        , getSeries
                                model.baseUrl
                                path
                        )

                DragStart from name ->
                    ( { model | currentDrag = Drag from name
                              , focus = Just from
                      }
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
                            let newTree = pasteSeries
                                            source
                                            destination
                                            series
                                            model.tree
                                transaction = buildTransaction
                                                source
                                                destination
                                                series
                                                model.currentTransactions
                            in
                            ( { model | tree = newTree
                                       , overDrag = Nothing
                                       , currentCut = NoCut
                                       , currentTransactions = transaction
                                }
                            , Task.perform
                                identity
                                <| Task.succeed
                                    <| ProcessTransaction
                                        0
                                        ( source, destination )
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

                QueryInput path queryInput ->
                    let
                        updatedModel = { model | tree =
                                            mutePayload
                                                path
                                                (\ p -> { p | queryInput = queryInput })
                                                model.tree
                                       }
                        debouncedMsg = FromTree (Query path queryInput)
                    in
                    Debouncer.update update queryDebouncerConfig
                        (Debouncer.provideInput debouncedMsg) updatedModel

                Restrict nb ->
                    U.nocmd { model | restriction = nb }

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
                                    , currentTransactions =
                                        buildTransaction
                                            from
                                            path
                                            series
                                            model.currentTransactions
                             }
                            , Task.perform
                                identity
                                <| Task.succeed
                                    <| ProcessTransaction
                                        0
                                        ( from, path )
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
                                            , queryInput = ""
                                            , submenuOpen = False
                                            , deleteConfirmOpen = False
                                            }
                                    )
                                    model.tree
                            , currentCut = NoCut
                        }

                PrepareDelete path ->
                    U.nocmd { model | tree =
                                mutePayload
                                    path
                                    (\p -> { p | deleteConfirmOpen = True })
                                    model.tree
                            }

                ConfirmDelete path ->
                    ( { model | openState =
                                    getOpenState
                                        model.tree
                      }
                    , deletePath
                        model.baseUrl
                        path
                    )

                CancelDelete path ->
                    U.nocmd { model | tree = closeAllMenus model.tree }

                CreationName path creationName ->
                    U.nocmd
                        { model | currentTyping =
                                    Creating
                                    path
                                    <| List.foldl
                                        (\ b -> String.replace
                                                b
                                                ""
                                        )
                                        creationName
                                        forbidden
                        }

                Create ->
                    case model.currentTyping of
                        NoTyping -> U.nocmd model
                        Renaming _ _ -> U.nocmd model
                        Creating path name ->
                            let prefix = case path of
                                            Root -> ""
                                            Branch p -> p ++ "."
                                            Unclassified -> "not possible"
                                newPath =  prefix ++  name
                                newPaths = model.paths
                                           ++
                                           [ newPath ]
                            in
                                ({ model | tree = initTree newPaths
                                         , paths = newPaths
                                         , currentTyping = NoTyping
                                  }
                                  , Cmd.batch
                                    ([ Task.perform
                                        identity
                                        ( Task.succeed ( FromTree ( Open path True False )))
                                     ] ++ ( reOpen ( getOpenState model.tree ))
                                    )

                                )

                RenameName path renameName ->
                    U.nocmd
                        { model | currentTyping =
                                    Renaming
                                    path
                                    <| String.replace
                                        "."
                                        ""
                                        renameName
                        }

                Rename ->
                    case model.currentTyping of
                        NoTyping -> U.nocmd model
                        Creating _ _ -> U.nocmd model
                        Renaming path name ->
                            case path of
                                Root -> U.nocmd model
                                Unclassified -> U.nocmd model
                                Branch oldBranch ->
                                    let splited = String.split "." oldBranch
                                        newBranch =
                                             case List.reverse splited of
                                                [ ] -> "" --at the root. Should not occur
                                                x :: xs ->
                                                    String.join
                                                        "."
                                                        <| List.reverse
                                                            <| [ name ] ++ xs
                                        newPaths =
                                            renamePaths oldBranch newBranch model.paths
                                    in
                                        ( { model | tree = initTree newPaths
                                                  , currentTyping = NoTyping
                                          }
                                        , Cmd.batch
                                            <| [ renameTreePath model.baseUrl oldBranch newBranch ]
                                            ++ ( reOpen
                                                <|Set.fromList
                                                    <| renamePaths
                                                        oldBranch
                                                        newBranch
                                                        <| Set.toList
                                                            ( getOpenState model.tree )
                                                )
                                        )

                SubMenu open path ->
                    U.nocmd { model | tree =
                                if open then
                                    mutePayload
                                        path
                                        (\p -> { p | submenuOpen = open })
                                        (closeAllMenus model.tree)
                                else
                                    closeAllMenus model.tree
                            }

                NoAction -> U.nocmd model




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


reOpen: Set String -> List ( Cmd Msg )
reOpen openState =
    List.map
        (\ dir ->
            Task.perform
                identity
                <| Task.succeed
                    ( FromTree
                        <| Open
                            ( pathFromString dir )
                            True
                            False
                    )
        )
        ( Set.toList openState )


updateSinglePath : String -> Int -> Path -> Path -> String -> Cmd Msg
updateSinglePath baseUrl idBatch source destination name =
    case destination of
        Root -> Cmd.none -- should not occur
        Branch dest ->
            case source of
                Root -> Cmd.none -- should not occur
                Branch _ ->
                    assignPath baseUrl idBatch source destination ( Just dest ) name
                Unclassified ->
                    assignPath baseUrl idBatch source destination ( Just dest ) name
        Unclassified ->
            case source of
                Root -> Cmd.none -- should not occur
                Unclassified -> Cmd.none -- no actual move
                Branch _ -> assignPath baseUrl idBatch source destination Nothing name


assignPath: String -> Int -> Path -> Path -> Maybe String -> String -> Cmd Msg
assignPath baseUrl idBatch source destination mDestination name =
    Http.request
        { method = "PATCH"
        , url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-path"]
                ([ UB.string "name" name
                 ] ++ case mDestination of
                        Nothing -> []
                        Just dest ->
                            [ UB.string "path" dest ]
                )
        , body = Http.emptyBody
        , expect = Http.expectString (GotAssignedPath idBatch source destination name )
        , headers = []
        , tracker = Nothing
        , timeout = Nothing
        }


removeFromQueue: Int -> Path -> Path -> String -> Model -> Model
removeFromQueue idBatch source destination name model =
    let s = reprPath source
        d = reprPath destination
    in
    case Dict.get (s, d) model.currentTransactions of
        Nothing -> model
        Just batchs ->
            case Dict.get idBatch batchs of
                Nothing -> model
                Just seriesList ->
                    let newQueue =
                            ( Set.toList
                                    ( Set.remove
                                        name
                                        ( Set.fromList seriesList )
                                    )
                            )
                        updatedTransactions =
                            if List.length newQueue > 0
                            then Dict.insert
                                    (s, d)
                                    ( Dict.insert idBatch newQueue batchs )
                                    model.currentTransactions
                            else
                                Dict.insert
                                    (s, d)
                                    ( Dict.remove idBatch batchs )
                                    model.currentTransactions
                    in
                    { model | currentTransactions = updatedTransactions }


buildTransaction: Path -> Path -> Set String -> Dict (String, String) ( Dict Int ( List String ))
                    -> Dict (String, String) ( Dict Int ( List String ))
buildTransaction source destination series current =
    if source == destination
    then Dict.empty
    else
        Dict.insert
            ( reprPath source, reprPath destination )
            ( splitByBatch 100 ( Set.toList series ) )
            current


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
        Unclassified ->
            let query = "(by.without-path)"
            in
            Http.get
                { url =
                    UB.crossOrigin
                        baseUrl
                        ["api", "series", "find"]
                        [ UB.string "query" query
                        , UB.string "sources" "local"
                        ]
                , expect = Http.expectString (GotSeries path)
                }
        Root ->
            Cmd.none



deletePath: String -> Path -> Cmd Msg
deletePath baseUrl path =
    case path of
        Root -> Cmd.none
        Unclassified -> Cmd.none
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


renameTreePath: String -> String -> String -> Cmd Msg
renameTreePath baseUrl oldBranch newBranch =
    Http.request
        { method = "PUT"
        , url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-path"]
                []
        , body = Http.jsonBody
                    <| JE.object
                        [ ( "path", JE.string oldBranch )
                        , ( "newpath", JE.string newBranch )
                        ]
        , expect = Http.expectString (GotRename oldBranch newBranch)
        , headers = [ ]
        , tracker = Nothing
        , timeout = Nothing
        }



view: Model -> Html Msg
view model =
    div
        [ class "menu-folders"
        , onClick (FromTree (SubMenu False Root))
        ]
        [ Html.h1
            [ class "page-title" ]
            [ Html.text "Series Tree" ]
        ,  Html.div
            [ class "horizon"
            , title "Selected horizon used in Quickview and Series Editor"
            ]
            [ Html.text "Selected horizon : "
            , selectHorizon model.horizon Horizon ]
            , viewTree
                model.baseUrl
                model.tree
                model.overDrag
                FromTree
                model.restriction
                model.focus
                model.currentCut
                model.currentTyping
            , viewMoving model.currentTransactions
            ]


viewMoving : Dict (String, String) (Dict Int ( List String ))-> Html Msg
viewMoving transactions =
    let countSeries dictBatch =
                    List.length
                        <| List.concat
                            <| Dict.values
                                dictBatch
        nbR = List.sum
                <| List.map
                       countSeries
                       <| Dict.values
                            transactions
    in
        if nbR == 0
        then Html.text ""
        else
            Html.div
                [ class "moving-warning" ]
                [ Html.p
                    []
                    [ Html.text
                        <| "Moving series : "
                        ++ String.fromInt nbR
                        ++ " remaining"
                    ]
            ]


queryDebouncerConfig =
    { mapMsg = DebounceQuery
    , getDebouncer = .queryDebouncer
    , setDebouncer = \deb model -> { model | queryDebouncer = deb }
    }


initModel: String -> Model
initModel baseUrl =
    let
        debouncerConfig =
            Debouncer.manual |>
            settleWhenQuietFor (Just <| fromSeconds 0.2) |>
            toDebouncer
    in
    { baseUrl = baseUrl
    , paths = []
    , tree = emptyTree
    , restriction = 150
    , currentDrag = NoDrag
    , overDrag = Nothing
    , errors = []
    , currentCut = NoCut
    , focus = Nothing
    , openState = Set.empty
    , currentTransactions = Dict.empty
    , currentTyping = NoTyping
    , queryDebouncer = debouncerConfig
    , horizon = initHorizon
                    baseUrl
                    ""
                    ""
                    ""
                    None
    }


sub: Model -> Sub Msg
sub model = Sub.batch
            [ cutSignal CopyFromBrowser
            , pasteSignal PasteFromBrowser
            , onKeyDown ( keyDecoder )
            , loadFromLocalStorage
                    (\ s-> Horizon (ModuleHorizon.FromLocalStorage s))
            ]


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
