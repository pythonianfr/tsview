module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD


type MyTree = Tree ( Dict String ( List MyTree ))

decodeTree: JD.Decoder ( List String )
decodeTree =
    JD.list JD.string


emptyTree: MyTree
emptyTree = Tree Dict.empty

stepTree: String -> MyTree -> MyTree
stepTree label previsouTree =
    Tree
    <| Dict.singleton
        label
        [ previsouTree ]

buildSingle : String -> MyTree
buildSingle path =
    List.foldr
        stepTree
        ( Tree Dict.empty )
        ( String.split "." path )

