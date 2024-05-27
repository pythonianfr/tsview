module TestUtil exposing (..)

import Expect
import Test


type alias T a =
    { name : String
    , input : a
    , output : String
    }

type alias TList a = List (T a)


buildTests : (a -> String) -> List (T a) -> List Test.Test
buildTests render = List.map (\x ->
    let
        res = Expect.equal
            (render x.input |> String.trim)
            (x.output |> String.trim)
    in Test.test x.name (always res))

buildTest : (a -> String) -> List (T a) -> Test.Test
buildTest f xs = buildTests f xs |> Test.concat 
