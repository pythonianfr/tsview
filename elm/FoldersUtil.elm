module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Attributes exposing
    ( class )
import Html.Events exposing
    ( onClick )
import Set exposing (Set)
import Tree
import Tree exposing
    ( Tree
    , indexedMap
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

root = "root"

type alias Payload =
    { name: String
    , position: Int
    , path: String
    , open: Bool
    , status: LoadingStatus
    , series: Set String
    }

initPayload: Payload
initPayload =
    { name= ""
    , position = -1
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
            <| convertTreeT
                <| MyTree ( Dict.singleton root myTree )


convertTreeT : MyTree -> List ( Tree Payload )
convertTreeT myTree =
    case myTree of
        MyTree dict ->
            List.map
                (\ (k, lmT) -> tree
                                {initPayload | name=k}
                                ( convertTreeT lmT ))
                ( Dict.toList dict )


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


toListItems : (Int -> msg) -> Payload -> List (Html msg) -> Html msg
toListItems  openMsg payload children =
    let open = payload.open
    in
    case children of
        [] ->
            Html.li
                []
                [ Html.p
                    [ class "folder"
                    , class ( if open then "open" else "not-open" )
                    , onClick ( openMsg payload.position )
                    ]
                    [ Html.text payload.name ] ]
        _ ->
            Html.li []
                ([ Html.p
                    [ class "folder"
                    , class ( if open then "open" else "not-open" )
                    , onClick ( openMsg payload.position )
                    ]
                    [ Html.text payload.name ]
                 ] ++ if open
                        then [Html.ul [] children]
                        else  []
                )

viewTree: Tree Payload -> (Int -> msg) -> Html msg
viewTree tree openMsg =
    Html.ul
        []
        [ restructure
            identity
            ( toListItems openMsg )
            tree
        ]


fillPostion: Tree Payload -> Tree Payload
fillPostion menu =
    indexedMap
        ( \idx payload -> { payload | position = idx} )
        menu


getZipper : Int -> Zipper Payload -> Maybe ( Zipper Payload )
getZipper idx menu =
    if (Tree.Zipper.label menu).position == idx
        then Just menu
        else
            case ( forward menu ) of
                Nothing -> Nothing
                Just nextStep ->
                    getZipper
                        idx
                        nextStep


getPayload: Int -> Tree Payload -> Payload
getPayload idx menu =
    Maybe.withDefault
        initPayload
        <| Maybe.map
            (Tree.Zipper.label)
            (getZipper idx (fromTree menu))


mutePayload : Int -> (Payload -> Payload) -> Tree Payload -> Tree Payload
mutePayload idx mapping menu =
    case getZipper idx ( fromTree menu ) of
        Nothing -> menu
        Just zipper
            -> toTree
                <| Tree.Zipper.mapLabel
                        mapping
                        zipper

