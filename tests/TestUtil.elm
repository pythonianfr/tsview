module TestUtil exposing (..)

import Expect
import Test


type alias T =
    { name : String
    , input : String
    , output : String
    }


type alias RenderInput = (String -> String)

buildTests : RenderInput -> List T -> List Test.Test
buildTests render xs = 
    List.map
        (\x ->
            let
                res =
                    Expect.equal (render x.input) (String.trim x.output)
            in
            Test.test x.name (always res)
        )
        xs
