port module Folders exposing (main)

import Browser
import Browser.Events exposing
    ( onKeyDown
    )
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Html exposing
    ( Html
    , Attribute
    , br
    , button
    , div
    , h2
    , input
    , li
    , p
    , text
    , ul
    )
import Html.Attributes exposing
    ( class
    , placeholder
    , title
    , type_
    , value
    )
import Html.Events exposing
    ( onClick
    , onInput )
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
    , MsgTree(..)
    , Path(..)
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
    , setOpenState
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
        _ -> Other keyValue

type ControlKey
    = Escape
    | Other String



type alias Model =
    { baseUrl: String
    , treeAttribute: Maybe String
    , treeAttributeInput: String
    , treeAttributeConfirmation: Bool
    , paths: List String
    , tree: Tree Payload
    , restriction: Int
    , currentDrag: Drag
    , overDrag: Maybe Path
    , errors: List String
    , currentCut: Cut
    , focus : Maybe Path
    , creationName: String
    , renameName: String
    , openState: Set String
    , currentTransactions : Dict (String, String) ( List String )
    , queryDebouncer: Debouncer Msg
    , horizon : HorizonModel
    }


type Msg
    = GotPaths ( Result Http.Error String )
    | GotTreeAttribute ( Result Http.Error String )
    | GotSetTreeAttribute ( Result Http.Error String )
    | GotSeries Path ( Result Http.Error String )
    | GotUpdatePath Path Path String ( Result Http.Error String )
    | TowardDeletion Path Path String ( Result Http.Error String )
    | GotDelete Path ( Result Http.Error String )
    | GotRename String String ( Result Http.Error String )
    | ProcessTransaction ( String, String )
    | TreeAttributeInput String
    | SubmitTreeAttribute
    | ConfirmTreeAttribute
    | CancelTreeAttribute
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
    , "-"
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

        GotSetTreeAttribute (Ok _) ->
            ( model
            , getTreeAttribute model.baseUrl
            )

        GotSetTreeAttribute (Err _) ->
            U.nocmd model

        TreeAttributeInput input ->
            U.nocmd { model | treeAttributeInput = input }

        SubmitTreeAttribute ->
            if String.trim model.treeAttributeInput == "" then
                U.nocmd model
            else
                U.nocmd { model | treeAttributeConfirmation = True }

        ConfirmTreeAttribute ->
            ( { model | treeAttributeConfirmation = False }
            , setTreeAttribute model.baseUrl model.treeAttributeInput
            )

        CancelTreeAttribute ->
            U.nocmd { model | treeAttributeConfirmation = False }

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

        GotUpdatePath source destination name (Ok _) ->
            let newModel = removeFromQueue source destination name model
            in
            ( newModel
            , if Dict.member
                    ( reprPath source, reprPath destination )
                    newModel.currentTransactions
              then Cmd.none
              else
                 Cmd.batch [ Task.perform
                                identity
                                ( Task.succeed ( FromTree ( Open source True False )))
                            , Task.perform
                                identity
                                ( Task.succeed ( FromTree ( Open destination True False )))
                            ]
            )

        GotUpdatePath source destination name (Err _ ) ->
           let newModel = removeFromQueue source destination name model
            in
            ( newModel
            , if Dict.member
                    ( reprPath source, reprPath destination )
                    newModel.currentTransactions
              then Cmd.none
              else
                 Cmd.batch [ Task.perform
                                identity
                                ( Task.succeed ( FromTree ( Open source True False )))
                            , Task.perform
                                identity
                                ( Task.succeed ( FromTree ( Open destination True False )))
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

        ProcessTransaction ( source, destination )
            -> let series = Dict.get
                                ( source, destination )
                                model.currentTransactions
               in
               case series of
                Nothing -> U.nocmd model
                Just listSeries ->
                    ( model
                    , Cmd.batch
                        <| List.map
                            ( updateSinglePath
                                model.baseUrl
                                model.treeAttribute
                                ( pathFromString source )
                                ( pathFromString destination )
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
                        Nothing -> U.nocmd { model | tree = closeAllMenus model.tree }
                        Just focus ->
                            ( { model | focus = Nothing
                                      , tree = closeAllMenus model.tree
                              },
                            Task.perform
                                identity
                                ( Task.succeed ( FromTree ( ButtonReset focus ))))

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
                                transaction = Dict.insert
                                                ( reprPath source, reprPath destination )
                                                ( Set.toList series )
                                                model.currentTransactions
                            in
                            ( { model | tree = newTree
                                       , overDrag = Nothing
                                       , currentTransactions = transaction
                                }
                            , Task.perform
                                identity
                                ( Task.succeed
                                    <| ProcessTransaction
                                        ( reprPath source, reprPath destination ))
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
                                            Dict.insert
                                                ( reprPath from, reprPath path )
                                                ( Set.toList series )
                                                model.currentTransactions
                             }
                            , Task.perform
                                identity
                                ( Task.succeed
                                    <| ProcessTransaction
                                        ( reprPath from, reprPath path ))
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

                CreationName creationName ->
                    U.nocmd
                        { model | creationName =
                                    List.foldl
                                        (\ b -> String.replace
                                                b
                                                ""
                                        )
                                        creationName
                                        forbidden

                        }

                Create path ->
                    if model.creationName == ""
                    then U.nocmd model
                    else
                    let prefix = case path of
                                    Root -> ""
                                    Branch p -> p ++ "."
                                    Unclassified -> "not possible"
                        newPath =  prefix ++  model.creationName
                        newPaths = model.paths
                                   ++
                                   [ newPath ]
                    in
                    ({ model | tree = initTree newPaths
                             , paths = newPaths
                             , creationName = ""
                      }
                      , Cmd.batch
                        ([ Task.perform
                            identity
                            ( Task.succeed ( FromTree ( Open path True False )))
                         ] ++ ( reOpen ( getOpenState model.tree ))
                        )

                    )

                RenameName renameName ->
                    U.nocmd
                        { model | renameName =
                                    String.replace
                                        "."
                                        ""
                                        renameName
                        }

                Rename path ->
                    if model.renameName == ""
                    then U.nocmd model
                    else
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
                                                    <| [ model.renameName ] ++ xs
                                newPaths =
                                    renamePaths oldBranch newBranch model.paths
                            in
                                ( { model | tree = initTree newPaths }
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


setTreeAttribute: String -> String -> Cmd Msg
setTreeAttribute baseUrl attribute =
    Http.request
        { method = "PUT"
        , url =
            UB.crossOrigin
                baseUrl
                ["api", "series", "tree-attribute"]
                []
        , body = Http.jsonBody
                    <| JE.object
                        [ ( "attribute", JE.string attribute ) ]
        , expect = Http.expectString GotSetTreeAttribute
        , headers = [ ]
        , tracker = Nothing
        , timeout = Nothing
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
                    ( GotUpdatePath source destination name )
        , headers = [ ]
        , tracker = Nothing
        , timeout = Nothing
        }


updateSinglePath : String -> Maybe String -> Path -> Path -> String -> Cmd Msg
updateSinglePath baseUrl treeAttribute source destination name =
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
                                    ( GotUpdatePath source destination name )
                        , headers = [ ]
                        , tracker = Nothing
                        , timeout = Nothing
                        }
                Unclassified ->
                    -- deletion of path attribute in the metadata
                    Http.request
                    { method = "GET"
                    , url =
                        UB.crossOrigin
                            baseUrl
                            ["api", "series", "metadata"]
                            [ UB.string "name" name ]
                    , body = Http.emptyBody
                    , expect = Http.expectString
                                ( TowardDeletion source Unclassified name )
                    , headers = [ ]
                    , tracker = Nothing
                    , timeout = Nothing
                    }
                Root ->
                    -- should not occur
                    Cmd.none


removeFromQueue: Path -> Path -> String -> Model -> Model
removeFromQueue source destination name model =
    let s = reprPath source
        d = reprPath destination
    in
    case Dict.get (s, d) model.currentTransactions of
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
                            newQueue
                            model.currentTransactions
                    else
                        Dict.remove
                            (s, d)
                            model.currentTransactions
            in
            { model | currentTransactions = updatedTransactions }


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
    case model.treeAttribute of
        Nothing -> 
            viewTreeAttributeSetup model
        Just _ ->
            div
                [ class "menu-folders"
                , onClick (FromTree (SubMenu False Root))
                ]
                [ Html.div
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
                , viewMoving model.currentTransactions
                ]


viewTreeAttributeSetup: Model -> Html Msg
viewTreeAttributeSetup model =
    div
        [ class "menu-folders tree-attribute-setup" ]
        [ div
            [ class "card" ]
            ( if model.treeAttributeConfirmation then
                viewTreeAttributeConfirmation model
              else
                viewTreeAttributeForm model
            )
        ]


viewTreeAttributeForm: Model -> List (Html Msg)
viewTreeAttributeForm model =
    [ h2 [] [ text "Setup Tree Attribute" ]
    , viewTreeAttributeInstructions
    , div
        [ class "tree-attribute-form" ]
        [ div
            [ class "form-group" ]
            [ input
                [ type_ "text"
                , placeholder "Enter attribute name (e.g., 'path', 'folder')"
                , value model.treeAttributeInput
                , onInput TreeAttributeInput
                , class "tree-attribute-input"
                ]
                []
            ]
        , button
            [ onClick SubmitTreeAttribute
            , class "tree-attribute-submit"
            ]
            [ text "Submit" ]
        ]
    ]


viewTreeAttributeConfirmation: Model -> List (Html Msg)
viewTreeAttributeConfirmation model =
    [ h2 [] [ text "Confirm Tree Attribute" ]
    , viewTreeAttributeConfirmationInstructions model
    , div
        [ class "tree-attribute-form" ]
        [ div
            [ class "confirmation-buttons" ]
            [ button
                [ onClick CancelTreeAttribute
                , class "tree-attribute-cancel"
                ]
                [ text "Cancel" ]
            , button
                [ onClick ConfirmTreeAttribute
                , class "tree-attribute-confirm"
                ]
                [ text "Confirm" ]
            ]
        ]
    ]


viewTreeAttributeInstructions: Html Msg
viewTreeAttributeInstructions =
    p [] [ text "Please define the attribute name that will be used to organize series in a tree structure. This will determine how your time series data is categorized and displayed in the folder tree." ]


viewTreeAttributeConfirmationInstructions: Model -> Html Msg
viewTreeAttributeConfirmationInstructions model =
    div []
        [ p []
            [ text "Are you sure you want to set the tree attribute to: "
            , Html.strong [] [ text ("\"" ++ model.treeAttributeInput ++ "\"") ]
            , text "?"
            ]
        , p [] [ text "This will determine how your time series data is organized. This setting can be changed later if needed." ]
        ]


viewMoving : Dict (String, String) ( List String )-> Html Msg
viewMoving transactions =
    let remaining = List.concat
                    ( Dict.values transactions )
        nbR = List.length remaining
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
    , treeAttribute = Nothing
    , treeAttributeInput = ""
    , treeAttributeConfirmation = False
    , paths = []
    , tree = emptyTree
    , restriction = 150
    , currentDrag = NoDrag
    , overDrag = Nothing
    , errors = []
    , currentCut = NoCut
    , focus = Nothing
    , creationName = ""
    , renameName = ""
    , openState = Set.empty
    , currentTransactions = Dict.empty
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