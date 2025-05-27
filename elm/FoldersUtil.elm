module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Attributes exposing
    ( attribute
    , checked
    , class
    , draggable
    , dropzone
    , type_
    , style
    )
import Html.Events as Events
import Html.Events exposing
    ( on
    , onCheck
    , onClick
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


zHeight path =
 if path == "." ++ "root"
 then "0"
 else
     String.fromInt
        <| String.length
            path

toListItems : Maybe Path ->(Path -> Bool -> msg) ->  (String -> Path -> msg) -> ( Path-> msg)  -> msg -> (Path -> msg)
               -> Payload -> List (Html msg) -> Html msg
toListItems overDrag openMsg dragStart dragOver dragEnd drop payload children =
    let open = payload.open
    in
        Html.li
            [ class "folder-and-series"
            , attribute "path" payload.path
            , style "z-index" (zHeight payload.path)
            ]
            ([ viewFolder overDrag openMsg dragOver drop payload open
             ]++ if open
                    then [ Html.ul
                            [ class "sub-folders-list" ]
                            children
                         , viewSeries overDrag payload dragStart dragEnd dragOver drop
                         ]
                    else  []
            )


viewFolder: Maybe Path -> (Path -> Bool -> msg) -> ( Path-> msg) -> ( Path-> msg) -> Payload -> Bool -> Html msg
viewFolder overDrag openMsg dragOver drop payload open =
   Html.p
    [ class "folder-name"
    , class ( if open then "open" else "not-open" )
    , class ( classOver payload overDrag )
    , onClick ( openMsg payload.path True )
    , onDragOver (dragOver payload.path)
    , onDrop ( drop payload.path )
    ]
    [ buttonOpen openMsg payload
    , Html.text payload.name
    ]


viewSeries : Maybe Path-> Payload -> (String -> Path -> msg) ->  msg -> ( Path-> msg) -> ( Path-> msg)  ->Html msg
viewSeries overDrag payload dragStart dragEnd dragOver drop =
    Html.ul
        [ class "series-list"]
        <| List.map
            (\sn -> Html.li
                        [ class "series-item" ]
                        [Html.p
                            [ class "series-name"
                            , draggable "true"
                            , class ( classOver payload overDrag )
                            , onDragStart
                                <| dragStart
                                    sn
                                    payload.path
                            , onDragOver ( dragOver payload.path )
                            , onDrop ( drop payload.path )
                            ]
                            [ Html.text sn ]]
            )
            <| Set.toList payload.series


buttonOpen: (Path -> Bool -> msg) -> Payload -> Html msg
buttonOpen openMsg payload =
    Html.input
        [ onCheck (openMsg payload.path)
        , class "folder-opener"
        , type_ "checkbox"
        , checked payload.open
        ]
        []


classOver: Payload -> Maybe Path -> String
classOver payload overDrag =
    case overDrag of
        Nothing -> ""
        Just path ->
            if payload.path == path
            then "targeted"
            else ""


viewTree: Tree Payload -> Maybe Path -> (Path -> Bool -> msg) -> (String -> Path -> msg) -> (Path -> msg) ->  msg
           -> (Path -> msg) -> Html msg
viewTree tree overDrag openMsg dragStart dragHover dragStop drop =
    Html.ul
        [class "folders-list"]
        [ restructure
            identity
            ( toListItems overDrag openMsg dragStart dragHover dragStop drop )
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

