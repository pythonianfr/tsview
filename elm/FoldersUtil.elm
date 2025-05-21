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
