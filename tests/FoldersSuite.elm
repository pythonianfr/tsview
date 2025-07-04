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
    , Path(..)
    , buildMTree
    , buildSingle
    , convertTree
    , decodeFind
    , decodeTree
    , getOpenState
    , getPayload
    , getZipper
    , initPayload
    , mergeMBranch
    , mutePayload
    , pasteSeries
    , root
    , selectFromUser
    , setOpenState
    , unclassifiedPayload
    )

unclassTree = tree
                unclassifiedPayload
                []

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

decodeFindResult : T.Test
decodeFindResult =
    let jsonStuff = """[{"name": "Belgium", "imeta": null, "meta": null, "source": "local", "kind": "primary"}]"""
        parsed =
            case JD.decodeString decodeFind jsonStuff of
                Err _ -> []
                Ok val -> val
    in
    T.test
        "Decode find"
        ( \ _ -> Expect.equal
                    parsed
                    [ "Belgium" ]
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
                    ( tree { initPayload | name = root
                                         , path = Root
                           }
                        [tree { initPayload | name = "a0"
                                            , path = Branch "a0"
                              }
                            [tree { initPayload | name = "b0"
                                                , path = Branch "a0.b0"
                                  }
                                [tree { initPayload | name = "c0"
                                                    , path = Branch "a0.b0.c0"
                                      }
                                    []
                                ]
                            ]
                        , unclassTree
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
                ( tree { initPayload | name = root
                                     , path = Root
                       }
                    [tree { initPayload | name = "a0"
                                        , path = Branch  "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                  }
                                []
                            ]
                        ]
                    , unclassTree
                    ]
                )
        )
    , T.test "Merge branch12"
        (\_ -> Expect.equal
                merge12
                ( tree { initPayload | name = root
                                     , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                   }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                                }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b1.c0"
                                  }
                                []
                            ]
                        ]
                    , unclassTree
                    ]
                )
        )
    ,T.test "Merge branch10"
        (\_ -> Expect.equal
                merge10
                ( tree { initPayload | name = root
                                     , path = Root
                        }
                    [tree { initPayload | name = "a0", path = Branch "a0"}
                        [tree { initPayload | name = "b0", path = Branch "a0.b0"}
                            [tree { initPayload | name = "c0", path = Branch "a0.b0.c0"}
                                []
                            ]
                        ]
                    , unclassTree
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
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                  }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                               }
                            []
                        ]
                    , tree { initPayload | name = "a1"
                                         , path = Branch "a1"
                           }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a1.b0"
                              }
                            []
                        ]
                    , unclassTree
                    ]
                )
            )
    ,    T.test "Build Tree step0"
            (\ _ ->
                Expect.equal
                 rTree0
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            []
                        ]
                    , unclassTree
                    ]
                )
            )
    ,   T.test "Build Tree step1"
            (\ _ ->
                Expect.equal
                 rTree1
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            []
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                               }
                            []
                        ]
                    , unclassTree
                    ]
                )
            )
    ,   T.test "Build Tree step2"
            (\ _ ->
                Expect.equal
                 rTree2
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                  }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                               }
                            []
                        ]
                    , unclassTree
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
         rTree = convertTree
                    <| buildMTree
                            paths
         selected0 =
            Maybe.map
                (Tree.Zipper.label)
                 (getZipper Root (fromTree rTree))
         selected1 =
                Maybe.map
                    (Tree.Zipper.label)
                     (getZipper ( Branch "a0" ) (fromTree rTree))
         selected4 =
                Maybe.map
                    (Tree.Zipper.label)
                     (getZipper ( Branch "a0.b1" ) (fromTree rTree))

     in
     T.concat
     [
        T.test "Fill Path Tree"
            (\ _ ->
                Expect.equal
                 rTree
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                  }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                               }
                            []
                        ]
                    , tree { initPayload | name = "a1"
                                         , path = Branch "a1"
                           }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a1.b0"
                              }
                            []
                        ]
                    , unclassTree
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
                                    , path = Branch "a0"
                      }
                )
        )
    ,
        T.test "Get node from position 0"
            (\ _ ->
                Expect.equal
                   selected0
                   <| Just { initPayload | name = root
                                         , path = Root
                            }
            )
    ,
        T.test "Get node from position 1"
            (\ _ ->
                Expect.equal
                   selected1
                   <| Just { initPayload | name = "a0"
                                         , path = Branch "a0"
                           }
            )
    ,
        T.test "Get node from position 4"
            (\ _ ->
                Expect.equal
                   selected4
                   <| Just { initPayload | name = "b1"
                                         , path = Branch "a0.b1"
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
         rTree = convertTree
                    <| buildMTree
                            paths
         modifiedTree = mutePayload
                            ( Branch "a0.b0.c0" )
                            (\ p ->
                                { p | open = True}
                            )
                            rTree
         newPayload = Maybe.map
                        (Tree.Zipper.label)
                        (getZipper ( Branch "a0.b0.c0" ) (fromTree modifiedTree))

     in
     T.test "Mute payload"
        (\_ -> Expect.equal
                newPayload
                <| Just
                    <| { initPayload | name = "c0"
                                     , path = Branch "a0.b0.c0"
                                     , open = True
                        }
        )


suiteCumulPath : T.Test
suiteCumulPath =
    let paths = [ "a0.b1"
                , "a0.b0.c0"
                ]
        rTree =  convertTree
                    <| buildMTree
                            paths
    in
        T.test "Other Fill Path"
            (\ _ ->
                Expect.equal
                 rTree
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [tree { initPayload | name = "a0"
                                        , path = Branch "a0"
                          }
                        [tree { initPayload | name = "b0"
                                            , path = Branch "a0.b0"
                              }
                            [tree { initPayload | name = "c0"
                                                , path = Branch "a0.b0.c0"
                                  }
                                []
                            ]
                        , tree { initPayload | name = "b1"
                                             , path = Branch "a0.b1"
                               }
                            []
                        ]
                    , unclassTree
                    ]
                 )
            )

suiteCopyPast : T.Test
suiteCopyPast =
    let paths = [ "b0"
                , "b1"
                ]
        selected = { selected = True }
        unselected = { selected = False }
        aTree =  convertTree
                    <| buildMTree
                            paths
        bTree = mutePayload
                    ( Branch "b0" )
                    (\ p -> { p | series =
                                    Dict.fromList
                                        [ ( "s0", selected )
                                        , ("s1", unselected )
                                        , ("s2", selected )
                                        ]
                            }
                    )
                    aTree
        cTree = mutePayload
                    ( Branch "b1" )
                    (\ p -> { p | series =
                                    Dict.singleton
                                        "s4"
                                        unselected
                            }
                    )
                    bTree
        cut = selectFromUser (getPayload ( Branch "b0" ) cTree)

        eTree = pasteSeries
                    ( Branch "b0" )
                    ( Branch "b1" )
                    cut
                    cTree
    in
         T.test "Cut and Paste"
            (\ _ ->
                Expect.equal
                 eTree
                 ( tree { initPayload | name = root
                                      , path = Root
                        }
                    [ tree { initPayload | name = "b0"
                                         , path = Branch "b0"
                                         , series =
                                            Dict.singleton
                                                "s1"
                                                unselected
                          }
                          []
                    , tree { initPayload | name = "b1"
                                         , path = Branch "b1"
                                         , series =
                                          Dict.fromList
                                            [ ("s4", unselected )
                                            , ("s0", unselected )
                                            , ("s2", unselected )
                                            ]
                          }
                          []
                    , unclassTree
                    ]
                )
            )

suiteKeepOpenState : T.Test
suiteKeepOpenState =
    let paths = ["a0.b0.c0"]
        rTree = convertTree
                    <| buildMTree
                            paths
        openTree =
            mutePayload
                ( Branch "a0.b0" )
                (\ p -> { p | open = True
                        }
                    )
                <| mutePayload
                    ( Branch "a0.b0.c0" )
                    (\ p -> { p | open = True
                        }
                    )
                    rTree
        newTree = convertTree
                    <| buildMTree
                            paths
        openState = getOpenState
                        openTree
        openedTree = setOpenState
                        newTree
                        openState
    in
        T.concat
        [ T.test "Open state"
            (\ _ ->
                Expect.equal
                    openState
                    ( Set.fromList ["a0.b0", "a0.b0.c0"] )
            )
        , T.test "Store open state"
             (\ _ ->
                Expect.equal
                    openTree
                    openedTree
             )
        , T.test "Validation open state"
             (\ _ ->
                Expect.notEqual
                    newTree
                    openedTree
             )
        ]
