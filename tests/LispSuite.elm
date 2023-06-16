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
                  (Ok [ Int 42])
        run4 =
            \_ -> Expect.equal
                  (parse "(foo bar quux)")
                  (Ok [ Symbol "foo", Symbol "bar", Symbol "quux" ])
        run5 =
            \_ -> Expect.equal (parse "(foo.bar)") (Ok ([ Symbol "foo.bar" ]))
        run6 =
            \_ -> Expect.equal
                  (parse "(foo \"hello\")")
                  (Ok [ Symbol "foo", String "hello" ])
        run7 =
            \_ -> Expect.equal
                  (parse "(\"hello\" \"world\")")
                  (Ok [ String "hello", String "world" ])
        run8 =
            \_ -> Expect.equal
                  (parse "(42.3)")
                  (Ok [ Float 42.3])
    in
    Test.concat
        [ test "lisp1" run1
        , test "lisp2" run2
        , test "lisp3" run3
        , test "lisp4" run4
        , test "lisp5" run5
        , test "lisp6" run6
        , test "lisp7" run7
        , test "lisp8" run8
        ]
