module FoldersSuite exposing (..)

import Expect exposing (Expectation)
import Test as T

import Json.Decode as JD

import FoldersUtil exposing
    (decodeTree)


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
