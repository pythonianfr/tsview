module LispSuite exposing
    ( testDepth
    , testLispParser
    , testParsing
    , testWidth
    )

import Expect
import Parser exposing (Parser)
import Test exposing (Test, test)
import Lisp exposing
    ( Atom(..)
    , Expr(..)
    , deadendstostr
    , depth
    , lispparser
    , serialize
    , width
    )


testLispParser : Test
testLispParser =
    let
        parse input = Parser.run lispparser input

        run1 =
            \_ -> Expect.equal (parse "(foo)") (Ok <| Expression [ Atom <| Symbol "foo" ])
        run2 =
            \_ -> Expect.equal
                  (parse "  (foo)  ")
                  (Err [{ col = 1, problem = Parser.ExpectingSymbol "(", row = 1 }])
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
                       [ Atom (Symbol "add")
                       , Expression [ Atom (Symbol "number")
                                    , Atom (Float 42.3)
                                    ]
                       , Expression [ Atom (Symbol "fibonacci")
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
                       ]
                  )
        run13 =
            \_ -> Expect.equal
                  (parse "(+ 2 (* 6.7 (/ pi 2)))")
                  (Ok <| Expression
                       [ Atom (Symbol "+")
                       , Atom (Int 2)
                       , Expression
                             [ Atom (Symbol "*")
                             , Atom (Float 6.7)
                             , Expression
                                 [ Atom (Symbol "/")
                                 , Atom (Symbol "pi")
                                 , Atom (Int 2)
                                 ]
                             ]
                       ]
                  )
        run14 =
            \_ -> Expect.equal
                  (parse "(> 2 3)")
                  (Ok <| Expression
                       [ Atom (Symbol ">")
                       , Atom (Int 2)
                       , Atom (Int 3)
                       ]
                  )
        run15 =
            \_ -> Expect.equal
                  (parse "(< 2 3)")
                  (Ok <| Expression
                       [ Atom (Symbol "<")
                       , Atom (Int 2)
                       , Atom (Int 3)
                       ]
                  )
        run16 =
            \_ -> Expect.equal
                  (parse "(<= 2 3)")
                  (Ok <| Expression
                       [ Atom (Symbol "<=")
                       , Atom (Int 2)
                       , Atom (Int 3)
                       ]
                  )
        run17 =
            \_ -> Expect.equal
                  (parse "(>= 2 3)")
                  (Ok <| Expression
                       [ Atom (Symbol ">=")
                       , Atom (Int 2)
                       , Atom (Int 3)
                       ]
                  )
        run18 =
            \_ -> Expect.equal
                  (parse "(** (series \"foo\") 4)")
                  (Ok <| Expression [ Atom <| Symbol "**"
                                    , Expression [ Atom <| Symbol "series"
                                                 , Atom <| String "foo"
                                                 ]
                                    , Atom <| Int 4
                                    ]
                  )
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
        , test "lisp13" run13
        , test "lisp14" run14
        , test "lisp15" run15
        , test "lisp16" run16
        , test "lisp17" run17
        , test "lisp18" run18
        ]


testDepth : Test
testDepth =
    let
        f1 = "(foo)"
        f2 = "(foo bar)"
        f3 = "(foo (bar quux) 42)"
        f4 = "(foo (bar (quux #:nope nil)))"

        parse input =
            Parser.run lispparser input

        run expect formula =
            Expect.equal expect <|
                case parse formula of
                    Ok expr -> (depth expr)
                    Err err -> -1
    in
    Test.concat
        [ test "depth1" (\_ -> run 1 f1)
        , test "depth2" (\_ -> run 1 f2)
        , test "depth3" (\_ -> run 2 f3)
        , test "depth4" (\_ -> run 3 f4)
        ]


testWidth : Test
testWidth =
    let
        f1 = "(foo)"
        f2 = "(foo bar)"
        f3 = "(foo (bar quux) 42.3 1)"
        f4 = "(foo #t (bar (quux #:nope nil)))"

        parse input =
            Parser.run lispparser input

        run expect formula =
            Expect.equal expect <|
                case parse formula of
                    Ok expr -> (width expr)
                    Err err -> -1
    in
    Test.concat
        [ test "width1" (\_ -> run (String.length f1) f1)
        , test "width2" (\_ -> run (String.length f2) f2)
        , test "width3" (\_ -> run (String.length f3) f3)
        , test "width4" (\_ -> run (String.length f4) f4)
        ]


type alias T =
    { name : String
    , input : String
    , output : String
    }


lisptests : List T
lisptests =
    [ T "+ spurious spaces" "( +   2.  6.7 )" "(+ 2 6.7)"
    , T "+ spurious spaces with keywords"
        "( + 2   #:flag #t  #:b (+ 1 6))"
        "(+ 2 #:flag #t #:b (+ 1 6))"
    , T "+ many args" "( +  3  4  5 )" "(+ 3 4 5)"
    , T "@ unsupported op"
        "(@ 3 4)"
        "FAIL: Expecting Symbol `)` at row 1 col 2"
    , T "* scientific notation"
        "(* -9.26e-08 #:b (+ 9.259e-02 -109))"
        "(* -9.26e-08 #:b (+ 0.09259 -109))"
    , T "priority OK" "( priority  4 3  6 7 )" "(priority 4 3 6 7)"
    , T "priority series OK"
        "( priority  (* 4 9) (* 3 (+ 2 6)) ( + 7 1 #:flag #t))"
        "(priority (* 4 9) (* 3 (+ 2 6)) (+ 7 1 #:flag #t))"
    , T "#flag #t OK" "(+  #:b 4 #:a 3  #:flag  #t)" "(+ #:b 4 #:a 3 #:flag #t)"
    , T "#flag duplicated"
        "(+  3 4  #:flag  nil  #:flag #f)"
        "(+ 3 4 #:flag nil #:flag #f)"
    , T "#k1, k2 OK"
        "(priority  3  4 10  nil  #:k2  7.3 #:k1  \"x\" )"
        """(priority 3 4 10 nil #:k2 7.3 #:k1 "x")"""
    ]


testParsing : Test
testParsing =
    let
        parseandserialize input output =
            case Parser.run lispparser input of
                Ok parsed -> serialize parsed
                Err err -> "FAIL: " ++ (deadendstostr err)
    in
    List.map
        (\item ->
            let
                res =
                    Expect.equal
                        (parseandserialize item.input item.output)
                        (String.trim item.output)
            in
            test item.name (always res)
        )
        lisptests |> Test.concat
