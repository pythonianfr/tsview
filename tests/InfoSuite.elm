module InfoSuite exposing (testMetaVal)

import Either
import Expect
import Info exposing (MetaVal(..), decodemetaval)
import Json.Decode as D
import Test exposing (Test, test)


type alias T =
    { name : String
    , json : String
    , result : MetaVal
    }


tests : List T
tests =
    [ T
        "Decode MInt"
        "7"
        (MInt 7)
    , T
        "Decode MList"
        """
[
    7,
    1.2,
    "test",
    true,
    [ "key", false, 1e-3 ]
]
"""
        (MList
            [ MInt 7
            , MFloat 1.2
            , MString "test"
            , MBool True
            , MList [ MString "key", MBool False, MFloat 1.0e-3 ]
            ]
        )
    ]


testMetaVal : Test
testMetaVal =
    let
        runTest : T -> Expect.Expectation
        runTest t =
            D.decodeString decodemetaval t.json
                |> Either.fromResult
                |> Either.unpack
                    (D.errorToString >> Expect.fail)
                    (Expect.equal t.result)
    in
    List.map (\t -> test t.name (\_ -> runTest t)) tests |> Test.concat
