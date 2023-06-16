module LispSuite exposing (testLispParser)

import Expect
import Parser exposing (Parser, Problem(..))
import Test exposing (Test, test)
import Lisp exposing (lispparser, Atom(..), Expr(..))


testLispParser : Test
testLispParser =
    let
        parse input = Parser.run lispparser input

        run1 =
            \_ -> Expect.equal (parse "(foo)") (Ok <| Expression [ Atom <| Symbol "foo" ])
        run2 =
            \_ -> Expect.equal
                  (parse "  (foo)  ")
                  (Err [{ col = 1, problem = ExpectingSymbol "(", row = 1 }])
        run3 =
            \_ -> Expect.equal
                  (parse "(42)")
                  (Ok <| Expression [ Atom <| Int 42 ])
        run4 =
            \_ -> Expect.equal
                  (parse "(foo bar quux)")
                  (Ok <| Expression
                       [ Atom <| Symbol "foo"
                       , Atom <| Symbol "bar"
                       , Atom <| Symbol "quux"
                       ]
                  )
        run5 =
            \_ -> Expect.equal
                  (parse "(foo.bar)")
                  (Ok <| Expression [ Atom <| Symbol "foo.bar" ])
        run6 =
            \_ -> Expect.equal
                  (parse "(foo \"hello\")")
                  (Ok <| Expression [ Atom <| Symbol "foo", Atom <| String "hello" ])
        run7 =
            \_ -> Expect.equal
                  (parse "(\"hello\" \"world\")")
                  (Ok <| Expression [ Atom <| String "hello", Atom <| String "world" ])
        run8 =
            \_ -> Expect.equal
                  (parse "(42.3)")
                  (Ok <| Expression [ Atom <| Float 42.3])
        run9 =
            \_ -> Expect.equal
                  (parse "(foo nil)")
                  (Ok <| Expression [ Atom <| Symbol "foo", Atom <| Nil ])
        run10 =
            \_ -> Expect.equal
                  (parse "(#t #f)")
                  (Ok <| Expression [ Atom <| Bool True, Atom <| Bool False ] )
        run11 =
            \_ -> Expect.equal
                  (parse "(add (number 42.3) (fibonacci 7))")
                  (Ok <| Expression
                       [ Atom (Symbol "add"),
                             Expression [ Atom (Symbol "number")
                                        , Atom (Float 42.3)
                                        ],
                             Expression [ Atom (Symbol "fibonacci")
                                        , Atom (Int 7)
                                        ]
                       ]
                  )
        run12 =
            \_ -> Expect.equal
                  (parse "(foo #:kw1 42 #:kw2 \"hello\"))")
                  (Ok <| Expression
                       [ Atom (Symbol "foo")
                       , Atom (Keyword "kw1")
                       , Atom (Int 42)
                       , Atom (Keyword "kw2")
                       , Atom (String "hello")
                       ])
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
        , test "lisp9" run9
        , test "lisp10" run10
        , test "lisp11" run11
        , test "lisp12" run12
        ]
