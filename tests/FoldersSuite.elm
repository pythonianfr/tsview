module FoldersSuite exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Set exposing (Set)
import Test as T

import Json.Decode as JD

import FoldersUtil exposing
    ( MyTree(..)
    , buildSingle
    , decodeTree
    , emptyTree
    , stepTree
    )


decodeTreeFolders : T.Test
decodeTreeFolders =
    let jsonStuff = """["a0","a0.b0","a0.b1","a0.b0.c0","a1.b0"]"""
        parsed =
            case JD.decodeString decodeTree jsonStuff of
                Err _ -> []
                Ok val -> val
    in
    T.test
        "Decode paths"
        ( \ _ -> Expect.equal
                    parsed
                    ["a0"
                    , "a0.b0"
                    , "a0.b1"
                    , "a0.b0.c0"
                    , "a1.b0"
                    ]
        )


buildSinglePath : T.Test
buildSinglePath =
    let path = "a0.b0.c0"
        ds = Dict.singleton
        ss = Set.singleton
        myTree = buildSingle path
    in
    T.test "Build branch"
        ( \ _ -> Expect.equal
                    myTree
                    ( buildSingle path )
        )