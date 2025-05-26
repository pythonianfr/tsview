module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Attributes exposing
    ( checked
    , class
    , draggable
    , dropzone
    , type_
    )
import Html.Events as Events
import Html.Events exposing
    ( on
    , onCheck
    )
import Set exposing (Set)
import Tree
import Tree exposing
    ( Tree
    , indexedMap
    , mapAccumulate
    , restructure
    , tree
    )
import Tree.Zipper
import Tree.Zipper exposing
    ( Zipper
    , forward
    , fromTree
    , toTree
    )


type MyTree = MyTree ( Dict String  MyTree )
type alias Path = String

root = "root"

type alias Payload =
    { name: String
    , path: Path
    , open: Bool
    , status: LoadingStatus
    , series: Set String
    }

initPayload: Payload
initPayload =
    { name= ""
    , path = ""
    , open = False
    , status = Start
    , series = Set.empty
    }

type LoadingStatus =
    Start
    | Loading
    | Success
    | LoadingError


-- ref for drag/drop: https://benpaulhanna.com/basic-html5-drag-and-drop-with-elm.html

onDragStart msg =
    Events.on "dragstart"
        <| JD.succeed msg

onDragEnd msg =
    Events.on "dragend"
        <| JD.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover"
        <| JD.succeed (msg, True)

onDrop msg =
    Events.preventDefaultOn "drop"
        <| JD.succeed (msg, True)


decodeTree: JD.Decoder ( List String )
decodeTree =
    JD.list JD.string


emptyMTree = MyTree Dict.empty

emptyTree: Tree Payload
emptyTree = tree initPayload []


stepTree: String -> MyTree -> MyTree
stepTree label previsouTree =
    MyTree
    <| Dict.singleton
        label
        previsouTree

buildSingle : String -> MyTree
buildSingle path =
    List.foldr
        stepTree
        ( MyTree Dict.empty )
        ( String.split "." path )


convertTree : MyTree -> Tree Payload
convertTree myTree =
    Maybe.withDefault
        ( tree initPayload [] )
        <| List.head
            <| convertTreeT ""
                <| MyTree ( Dict.singleton root myTree )


convertTreeT : String -> MyTree -> List ( Tree Payload )
convertTreeT path myTree =
    case myTree of
        MyTree dict ->
            List.map
                ( recConvert path )
                ( Dict.toList dict )


recConvert: Path -> ( String,  MyTree ) -> Tree Payload
recConvert path ( name,  subTree) =
    let newPath = String.replace
                    ".root."
                    ""
                    ( path ++ "." ++ name )
    in
    tree
        { initPayload | name=name
                      , path = newPath
        }
        ( convertTreeT newPath subTree )


unpack: MyTree -> ( Dict String  MyTree )
unpack mTree =
    case mTree of
        MyTree dict -> dict

mergeMBranch: MyTree -> MyTree -> MyTree
mergeMBranch branch tree =
    let dictT = unpack tree
        dictB = unpack branch
    in
        case Dict.toList dictB of
            [] -> tree
            (k, v) :: xs ->
                case Dict.get k dictT of
                    Nothing -> MyTree ( Dict.insert k v dictT )
                    Just subTree -> MyTree
                                        <| Dict.insert
                                                k
                                                (mergeMBranch v subTree)
                                                dictT

buildMTree : List String -> MyTree
buildMTree paths =
    let branchs = List.map buildSingle paths
    in
        List.foldr
            mergeMBranch
            emptyMTree
            branchs

buildTree : List String -> Tree Payload
buildTree paths =
    convertTree
        <| buildMTree paths


toListItems : (Path -> Bool -> msg) ->  (String -> Path -> msg) -> ( Path-> msg)  -> msg -> (Path -> msg)
               -> Payload -> List (Html msg) -> Html msg
toListItems openMsg dragStart dragOver dragEnd drop payload children =
    let open = payload.open
    in
        Html.li
            [ class "folder"
            , onDragOver (dragOver payload.path)
            , onDrop ( drop payload.path )
            ]
            ([ viewFolder openMsg payload open
             ]++ if open
                    then [ Html.ul [] children
                         , viewSeries payload dragStart dragEnd
                         ]
                    else  []
            )


viewFolder: (Path -> Bool -> msg) -> Payload -> Bool -> Html msg
viewFolder openMsg  payload open =
   Html.p
    [ class "folder-name"
    , class ( if open then "open" else "not-open" )

    ]
    [ buttonOpen openMsg payload
    , Html.text payload.name
    ]


viewSeries : Payload -> (String -> Path -> msg) ->  msg -> Html msg
viewSeries payload dragStart dragEnd =
    Html.ul
        []
        <| List.map
            (\sn -> Html.li [] [Html.p
                                [ draggable "true"
                                , onDragStart
                                    <| dragStart
                                        sn
                                        payload.path
                                , onDragEnd dragEnd
                                ]
                                [ Html.text sn ]]
            )
            <| Set.toList payload.series


buttonOpen: (Path -> Bool -> msg) -> Payload -> Html msg
buttonOpen openMsg payload =
    Html.input
        [ onCheck (openMsg payload.path)
        , type_ "checkbox"
        , checked payload.open
        ]
        []


viewTree: Tree Payload -> (Path -> Bool -> msg) -> (String -> Path -> msg) -> (Path -> msg) ->  msg
           -> (Path -> msg) -> Html msg
viewTree tree openMsg dragStart dragHover dragStop drop =
    Html.ul
        []
        [ restructure
            identity
            ( toListItems openMsg dragStart dragHover dragStop drop )
            tree
        ]


getZipper : String -> Zipper Payload -> Maybe ( Zipper Payload )
getZipper path menu =
    if (Tree.Zipper.label menu).path == path
        then Just menu
        else
            case ( forward menu ) of
                Nothing -> Nothing
                Just nextStep ->
                    getZipper
                        path
                        nextStep


getPayload: String -> Tree Payload -> Payload
getPayload path menu =
    Maybe.withDefault
        initPayload
        <| Maybe.map
            (Tree.Zipper.label)
            (getZipper path (fromTree menu))


mutePayload : String -> (Payload -> Payload) -> Tree Payload -> Tree Payload
mutePayload path mapping menu =
    case getZipper path ( fromTree menu ) of
        Nothing -> menu
        Just zipper
            -> toTree
                <| Tree.Zipper.mapLabel
                        mapping
                        zipper

