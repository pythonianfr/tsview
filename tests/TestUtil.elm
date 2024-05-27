module TestUtil exposing (..)

import Expect
import Test


type alias T a =
    { name : String
    , input : a
    , output : String
    }


buildTests : (a -> String) -> List (T a) -> List Test.Test
buildTests render = List.map (\x ->
    let
        res = Expect.equal
            (render x.input |> String.trim)
            (x.output |> String.trim)
    in Test.test x.name (always res))
