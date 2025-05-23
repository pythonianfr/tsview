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
import Tree.Zipper exposing
    ( forward
    , fromTree
    , toTree
    )

import Json.Decode as JD

import FoldersUtil exposing
    ( MyTree(..)
    , buildMTree
    , buildSingle
    , convertTree
    , decodeTree
    , fillPostion
    , getZipper
    , initPayload
    , mergeMBranch
    , mutePayload
    , root
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
                    ( tree { initPayload | name = root}
                        [tree { initPayload | name = "a0"}
                            [tree { initPayload | name = "b0"}
                                [tree { initPayload | name = "c0"}
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
                ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            [tree { initPayload | name = "c0"}
                                []
                            ]
                        ]
                    ]
                )
        )
    , T.test "Merge branch12"
        (\_ -> Expect.equal
                merge12
                ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            [tree { initPayload | name = "c0"}
                                []
                            ]
                        , tree { initPayload | name = "b1"}
                            [tree { initPayload | name = "c0"}
                                []
                            ]
                        ]
                    ]
                )
        )
    ,T.test "Merge branch10"
        (\_ -> Expect.equal
                merge10
                ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            [tree { initPayload | name = "c0"}
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
                 ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            [tree { initPayload | name = "c0"}
                                []
                            ]
                        , tree { initPayload | name = "b1"}
                            []
                        ]
                    , tree { initPayload | name = "a1"}
                        [tree { initPayload | name = "b0"}
                            []
                        ]
                    ]
                )
            )
    ,    T.test "Build Tree step0"
            (\ _ ->
                Expect.equal
                 rTree0
                 ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            []
                        ]
                    ]
                )
            )
    ,   T.test "Build Tree step1"
            (\ _ ->
                Expect.equal
                 rTree1
                 ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            []
                        , tree { initPayload | name = "b1"}
                            []
                        ]
                    ]
                )
            )
    ,   T.test "Build Tree step2"
            (\ _ ->
                Expect.equal
                 rTree2
                 ( tree { initPayload | name = root}
                    [tree { initPayload | name = "a0"}
                        [tree { initPayload | name = "b0"}
                            [tree { initPayload | name = "c0"}
                                []
                            ]
                        , tree { initPayload | name = "b1"}
                            []
                        ]
                    ]
                )
            )
    ]


suiteIndexPosition : T.Test
suiteIndexPosition =
     let paths = [ "a0"
                , "a0.b0"
                , "a0.b1"
                , "a0.b0.c0"
                , "a1.b0"
                ]
         rTree = fillPostion
                    <| convertTree
                        <| buildMTree
                                paths
         selected0 =
            Maybe.map
                (Tree.Zipper.label)
                 (getZipper 0 (fromTree rTree))
         selected1 =
                Maybe.map
                    (Tree.Zipper.label)
                     (getZipper 1 (fromTree rTree))
         selected4 =
                Maybe.map
                    (Tree.Zipper.label)
                     (getZipper 4 (fromTree rTree))

     in
     T.concat
     [
        T.test "Fill Position Tree"
            (\ _ ->
                Expect.equal
                 rTree
                 ( tree { initPayload | name = root
                                      , position = 0
                        }
                    [tree { initPayload | name = "a0"
                                        , position = 1
                          }
                        [tree { initPayload | name = "b0"
                                            , position = 2
                              }
                            [tree { initPayload | name = "c0"
                                                , position = 3
                                  }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , position = 4
                               }
                            []
                        ]
                    , tree { initPayload | name = "a1"
                                         , position = 5
                           }
                        [tree { initPayload | name = "b0"
                                            , position = 6
                              }
                            []
                        ]
                    ]
                )
            )
    , T.test "Forward in zipper"
        (\_ -> Expect.equal
                (Maybe.map
                    Tree.Zipper.label
                    ( forward (fromTree rTree) )
                )
                (Just { initPayload | name = "a0"
                                    , position = 1
                      }
                )
        )
    ,
        T.test "Get node from position 0"
            (\ _ ->
                Expect.equal
                   selected0
                   <| Just { initPayload | name = root
                                         , position = 0
                            }
            )
    ,
        T.test "Get node from position 1"
            (\ _ ->
                Expect.equal
                   selected1
                   <| Just { initPayload | name = "a0"
                                         , position = 1
                           }
            )
    ,
        T.test "Get node from position 4"
            (\ _ ->
                Expect.equal
                   selected4
                   <| Just { initPayload | name = "b1"
                                         , position = 4
                           }
            )
    ]


suiteMutatePayload : T.Test
suiteMutatePayload =
     let paths = [ "a0"
                , "a0.b0"
                , "a0.b1"
                , "a0.b0.c0"
                , "a1.b0"
                ]
         rTree = fillPostion
                    <| convertTree
                        <| buildMTree
                                paths
         modifiedTree = mutePayload
                            3
                            (\ p ->
                                { p | open = True}
                            )
                            rTree
         newPayload = Maybe.map
                        (Tree.Zipper.label)
                        (getZipper 3 (fromTree modifiedTree))

     in
     T.test "Mute payload"
        (\_ -> Expect.equal
                newPayload
                <| Just
                    <| { initPayload | name = "c0"
                                     , position = 3
                                     , open = True
                        }
        )