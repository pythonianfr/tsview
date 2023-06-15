module LispSuite exposing (testLispParser)

import Expect
import Parser exposing (Parser, Problem(..))
import Test exposing (Test, test)
import Lisp exposing (parser, Atom(..))


testLispParser : Test
testLispParser =
    let
        parse input = Parser.run parser input

        run1 =
            \_ -> Expect.equal (parse "(foo)") (Ok [ Symbol "foo" ])
        run2 =
            \_ -> Expect.equal (parse "  (foo)  ") (Ok [ Symbol "foo" ])
        run3 =
            \_ -> Expect.equal
                  (parse "(42)")
                  (Err [{ col = 2, problem = ExpectingVariable, row = 1}])
    in
    Test.concat
        [ test "lisp1" run1
        , test "lisp2" run2
        , test "lisp3" run3
        ]
