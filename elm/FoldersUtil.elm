module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD
import Tree
import Tree exposing
    ( Tree
    , tree
    )

type MyTree = MyTree ( Dict String  MyTree )

decodeTree: JD.Decoder ( List String )
decodeTree =
    JD.list JD.string


emptyTree = MyTree Dict.empty

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


convertTree : MyTree -> Tree String
convertTree myTree =
    Maybe.withDefault
        ( tree "empty" [] )
        <| List.head
            <| convertTreeT
                <| MyTree ( Dict.singleton "root" myTree )


convertTreeT : MyTree -> List ( Tree String )
convertTreeT myTree =
    case myTree of
        MyTree dict ->
            ( List.map
                (\ (k, lmT) -> tree k ( convertTreeT lmT ))
                ( Dict.toList dict )
            )

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
            emptyTree
            branchs
