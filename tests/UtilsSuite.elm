module UtilsSuite exposing (testStringParser)

import Expect
import Parser exposing ((|.), (|=), Parser)
import Test exposing (Test, test)
import TsView.Formula.Utils exposing (stringParser)


testStringParser : Test
testStringParser =
    let
        input =
            """
"serie-2020"
"""

        inputParser : Parser String
        inputParser =
            Parser.succeed identity
                |. Parser.spaces
                |= stringParser
                |. Parser.spaces
                |. Parser.end

        run i r =
            \_ -> Expect.equal (Parser.run inputParser i) (Ok r)
    in
    Test.concat
        [ test "stringParser multi" (run input "serie-2020")
        , test "stringParser single" (run "\"s1.x\"" "s1.x")
        ]
