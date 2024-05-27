module OpticsExtra exposing (..)

import Either
import Maybe.Extra as Maybe

import List.Extra
import List.Nonempty as NE
import List.Nonempty.Ancillary as NEA
import RoseTree.Tree as Tree exposing (Tree)

import Optics.Basic as OB
import Optics.Core as O exposing (o)

import AssocList as Assoc


-- utils

has_ : O.Optic pr ls s t a b -> s -> Bool
has_ optics s = O.getSome optics s |> Maybe.isJust

only_ : (a -> Bool) -> O.SimpleTraversal a a
only_ = OB.only


-- Maybe
just_ : O.Prism pr (Maybe a) (Maybe b) a b
just_ = OB.just_

nothing_ : O.SimplePrism pr (Maybe a) ()
nothing_ = OB.nothing_

-- Tuple
first_ : O.Lens n ( a, c ) ( b, c ) a b
first_ = OB.first

second_ : O.Lens n ( c, a ) ( c, b ) a b
second_ = OB.second

-- List XXX uncons !
cons_ : O.Prism pr (List a) (List b) (a, List a) (b, List b)
cons_ = OB.cons

consLast_ : O.Prism pr (List a) (List b) (a, List a) (b, List b)
consLast_ = O.prism
    (\(x, xs) -> List.append xs [x])
    (\xs -> List.Extra.unconsLast xs |> Either.fromMaybe [])

listIx_ : Int -> O.SimpleTraversal (List a) a
listIx_ i = O.traversal
    (\xs -> List.Extra.getAt i xs |> Maybe.toList)
    (\f xs -> List.Extra.updateAt i f xs)

listHead_ : O.SimpleTraversal (List a) a
listHead_ = listIx_ 0

listLast_ : O.SimpleTraversal (List a) a
listLast_ = O.traversal
    (\xs -> O.getAll (listIx_ <| List.length xs - 1) xs)
    (\f xs -> O.over (listIx_ <| List.length xs - 1) f xs)

listEach_ : O.Traversal (List a) (List b) a b
listEach_ = O.traversal identity List.map


-- Nonempty

neIx_ : Int -> O.SimpleTraversal (NE.Nonempty a) a
neIx_ i = O.traversal
    (\xs -> NE.get i xs |> List.singleton)
    (\f xs -> NEA.updateAt i f xs)

neHead_ : O.SimpleTraversal (NE.Nonempty a) a
neHead_ = neIx_ 0

neLast_ : O.SimpleTraversal (NE.Nonempty a) a
neLast_ = O.traversal
    (\xs -> O.getAll (neIx_ <| NE.length xs - 1) xs)
    (\f xs -> O.over (neIx_ <| NE.length xs - 1) f xs)

neEach_ : O.Traversal (NE.Nonempty a) (NE.Nonempty b) a b
neEach_ = O.traversal NE.toList NE.map


-- Assoc

assocValues_ : O.SimpleTraversal (Assoc.Dict k v) v
assocValues_ = O.traversal
    (\assoc -> Assoc.toList assoc |> List.map Tuple.second)
    (\f assoc -> Assoc.map (\_ v -> f v) assoc)

-- Tree

type alias Forest a = List (Tree a)

type alias TreePath = List Int

node_ : O.SimpleLens ls (Tree a) a
node_ = O.lens Tree.value (\s a -> Tree.setValue a s)

forest_ : O.SimpleLens ls (Tree a) (Forest a)
forest_ = O.lens Tree.children <| \tree forest ->
    Tree.branch (Tree.value tree) forest

treeIx_ : TreePath -> O.SimpleTraversal (Tree a) (Tree a)
treeIx_ treePath = case treePath of
    [] -> O.traversal
        (\tree -> List.singleton tree)
        (\f tree -> f tree)
        
    _ -> O.traversal
        (\tree -> Tree.get treePath tree |> Maybe.toList)
        (\f tree -> Tree.updateAt treePath f tree)
