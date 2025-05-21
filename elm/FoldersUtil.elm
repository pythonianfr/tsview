module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Events exposing
    ( onClick )
import Set exposing (Set)
import Tree
import Tree exposing
    ( Tree
    , restructure
    , tree
    )

type MyTree = MyTree ( Dict String  MyTree )

type alias Payload =
    { label: String
    , position: Int
    , path: String
    , open: Bool
    , status: LoadingStatus
    , series: Set String
    }

initPayload: Payload
initPayload =
    { label= ""
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
                <| MyTree ( Dict.singleton "root" myTree )


convertTreeT : MyTree -> List ( Tree Payload )
convertTreeT myTree =
    case myTree of
        MyTree dict ->
            List.map
                (\ (k, lmT) -> tree
                                {initPayload | label=k}
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

labelToHtml : Payload -> Html msg
labelToHtml payload =
    Html.text payload.label

toListItems : msg -> Html msg -> List (Html msg) -> Html msg
toListItems openMsg label children =
    case children of
        [] ->
            Html.li
                []
                [ Html.p
                    [ onClick openMsg ]
                    [ label ]
                ]
        _ ->
            Html.li []
                [ Html.p
                    [ onClick openMsg ]
                    [ label ]
                , Html.ul [] children
                ]

viewTree: Tree Payload -> msg -> Html msg
viewTree tree openMsg =
    Html.ul
        []
        [ restructure
            labelToHtml
            ( toListItems openMsg )
            tree
        ]
