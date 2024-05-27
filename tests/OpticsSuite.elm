module OpticsSuite exposing (mainTest)

import Test

import List.Nonempty as NE
import RoseTree.Tree exposing (Tree, branch, leaf) 

import Optics.Core as O exposing (o)
import OpticsExtra as OE

import TestUtil exposing (T)


mainTest : Test.Test
mainTest = Test.concat
    [ listIxTest
    , neIxTest
    , treeIxTest
    ]


listIxTest : Test.Test
listIxTest = TestUtil.buildTest (render renderList) <|
    [ T "List Optics modification"
        ([8, 3, 7, 2, 9, 1]
            |> O.over OE.listHead_ ((+) 1)
            |> O.over (OE.listIx_ 4) (\x -> x - 1)
            |> O.assign OE.listLast_ 12
            |> O.assign (OE.listIx_ 3) 33
        )
        """
    9
    3
    7
    33
    8
    12
"""
    ]

neIxTest : Test.Test
neIxTest = TestUtil.buildTest (NE.toList >> (render renderList)) <|
    [ T "NE Optics modification"
        (NE.Nonempty 8 [3, 7, 2, 9, 1]
            |> O.over OE.neHead_ ((+) 1)
            |> O.over (OE.neIx_ 4) (\x -> x - 1)
            |> O.assign OE.neLast_ 12
            |> O.assign (OE.neIx_ 3) 33
        )
        """
    9
    3
    7
    33
    8
    12
"""
    ]

treeIxTest : Test.Test
treeIxTest =
    let
        devTree : Tree Int
        devTree =
            branch 3
                [ leaf 6
                , branch 7
                    [ leaf 9
                    , leaf 2
                    , leaf 2
                    ]
                , leaf 12
                ]

    in TestUtil.buildTest (render renderTree) <| 
    [ T "Tree Optics modification"
        (devTree
        |> O.assign (o (OE.treeIx_ []) OE.node_) 2
        |> O.assign (OE.treeIx_ [0]) (branch 5 [ leaf 3, leaf 1 ])
        |> O.assign (o (OE.treeIx_ [1, 0]) OE.node_) 8
        |> O.over (o (OE.treeIx_ [1, 2]) OE.node_) ((+) 1)
        |> O.assign (o (OE.treeIx_ [2]) OE.forest_) [ leaf 15, leaf 20 ]
        )
        """
2
    5
        3
        1
    7
        8
        2
        3
    12
        15
        20
"""
    ]


-- rendering utils

renderTree : Int -> Tree Int -> List (Int, String)
renderTree indent tree =
    (::)
    (indent, String.fromInt <| O.get OE.node_ tree)
    (List.concatMap (renderTree (indent + 1)) <| O.get OE.forest_ tree)

renderList : Int -> List Int -> List (Int, String)
renderList indent xs =
    List.map (String.fromInt >> Tuple.pair (indent + 1)) xs

render : (Int -> a -> List (Int, String)) -> a -> String
render f x =
   let tab = String.repeat 4 " "
   in String.join "\n" <| List.map
    (\(i, s) -> (String.repeat i tab) ++ s)
    (f 0 x)
