module FoldersSuite exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Set exposing (Set)
import Test as T
import Tree
import Tree exposing
    ( Tree
    , tree
    )

import Json.Decode as JD

import FoldersUtil exposing
    ( MyTree(..)
    , buildMTree
    , buildSingle
    , convertTree
    , decodeTree
    , mergeMBranch
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
        rTree = convertTree myTree
    in
    T.test "Build branch"
        ( \ _ -> Expect.equal
                    rTree
                    ( tree ( "root", False )
                        [tree ( "a0", False )
                            [tree ("b0", False )
                                [tree ("c0", False )
                                    []
                                ]
                            ]
                        ]
                    )
        )


suiteMergeMBranch : T.Test
suiteMergeMBranch =
    let branch0 = buildSingle "a0"
        branch1 = buildSingle "a0.b0.c0"
        branch2 = buildSingle "a0.b1.c0"
        merge01 = convertTree ( mergeMBranch branch0 branch1 )
        merge12 = convertTree ( mergeMBranch branch1 branch2 )
        merge10 = convertTree ( mergeMBranch branch1 branch0 )
    in
    T.concat
    [ T.test "Merge branch01"
        (\_ -> Expect.equal
                merge01
                ( tree ( "root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            [tree ("c0", False )
                                []
                            ]
                        ]
                    ]
                )
        )
    , T.test "Merge branch12"
        (\_ -> Expect.equal
                merge12
                ( tree ("root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            [tree ("c0", False )
                                []
                            ]
                        , tree ("b1", False )
                            [tree ("c0", False )
                                []
                            ]
                        ]
                    ]
                )
        )
    ,T.test "Merge branch10"
        (\_ -> Expect.equal
                merge10
                ( tree ("root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            [tree ("c0", False )
                                []
                            ]
                        ]
                    ]
                )
        )
    ]


suiteConvertTree : T.Test
suiteConvertTree =
    let paths = [ "a0"
                , "a0.b0"
                , "a0.b1"
                , "a0.b0.c0"
                , "a1.b0"
                ]
        mTree = buildMTree paths
        rTree = convertTree mTree
        step0 = [ "a0"
                , "a0.b0"
                ]
        rTree0 = convertTree ( buildMTree step0 )
        step1 = [ "a0"
                , "a0.b0"
                , "a0.b1"
                ]
        rTree1 = convertTree ( buildMTree step1 )
        step2 =  [ "a0"
                , "a0.b0"
                , "a0.b1"
                , "a0.b0.c0"
                ]
        rTree2 = convertTree ( buildMTree step2 )
    in
    T.concat
    [
        T.test "Build Tree"
            (\ _ ->
                Expect.equal
                 rTree
                 ( tree ("root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            [tree ("c0", False )
                                []
                            ]
                        , tree ("b1", False )
                            []
                        ]
                    , tree ("a1", False )
                        [tree ("b0", False )
                            []
                        ]
                    ]
                )
            )
    ,    T.test "Build Tree step0"
            (\ _ ->
                Expect.equal
                 rTree0
                 ( tree ("root", False)
                    [tree ("a0", False)
                        [tree ("b0", False)
                            []
                        ]
                    ]
                )
            )
    ,   T.test "Build Tree step1"
            (\ _ ->
                Expect.equal
                 rTree1
                 ( tree ("root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            []
                        , tree ("b1", False )
                            []
                        ]
                    ]
                )
            )
    ,   T.test "Build Tree step2"
            (\ _ ->
                Expect.equal
                 rTree2
                 ( tree ("root", False )
                    [tree ("a0", False )
                        [tree ("b0", False )
                            [tree ("c0", False )
                                []
                            ]
                        , tree ("b1", False )
                            []
                        ]
                    ]
                )
            )
    ]
