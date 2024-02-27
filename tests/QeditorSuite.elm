module QeditorSuite exposing ( testSerialize )

import Expect
import Test exposing (Test, test)

import Lisp exposing
    ( Atom(..)
    , Expr(..)
    )
import Queryeditor exposing
    ( FilterNode(..)
    , Value(..)
    , serialize
    )


testSerialize : Test
testSerialize =
    let
        run1 =
            \_ -> Expect.equal
                  (serialize TzAware)
                  (Expression [ Atom <| Symbol "by.tzaware" ])

        run2 =
            \_ -> Expect.equal
                  (serialize <| Not TzAware)
                  (Expression [ Atom <| Symbol "by.not"
                              , Expression [ Atom <| Symbol "by.tzaware" ]
                              ])

        run3 =
            \_ -> Expect.equal
                  (serialize <| Or [ TzAware , ByName "foo" ])
                  (Expression [ Atom <| Symbol "by.or"
                              , Expression
                                    [ Expression [ Atom <| Symbol "by.tzaware" ]
                                    , Expression [ Atom <| Symbol "by.name"
                                                 , Atom <| String "foo"
                                                 ]
                                    ]
                              ])

        run4 =
            \_ -> Expect.equal
                  (serialize <| And [ Or [ TzAware, Formula ] , ByName "foo" ])
                  (Expression
                       [ Atom (Symbol "by.and")
                       , Expression
                             [ Expression
                                   [ Atom (Symbol "by.or")
                                   , Expression
                                         [ Expression
                                               [ Atom (Symbol "by.tzaware") ]
                                         , Expression
                                               [ Atom (Symbol "by.formula") ]
                                         ]
                                   ]
                             , Expression
                                   [ Atom (Symbol "by.name")
                                   , Atom (String "foo")
                                   ]
                             ]
                       ])

        run5 =
            \_ -> Expect.equal
                  (serialize <| ByMetakey "key")
                  (Expression [ Atom <| Symbol "by.metakey"
                              , Atom <| String "key"
                              ]
                  )

        run6 =
            \_ -> Expect.equal
                  (serialize <| Eq "key" (Str "value"))
                  (Expression [ Atom <| Symbol "="
                              , Atom <| String "key"
                              , Atom <| String "value"
                              ]
                  )

        run7 =
            \_ -> Expect.equal
                  (serialize <| Eq "key" (Number 42))
                  (Expression [ Atom <| Symbol "="
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run8 =
            \_ -> Expect.equal
                  (serialize <| Gt "key" (Str "value"))
                  (Expression [ Atom <| Symbol ">"
                              , Atom <| String "key"
                              , Atom <| String "value"
                              ]
                  )

        run9 =
            \_ -> Expect.equal
                  (serialize <| Gt "key" (Number 42))
                  (Expression [ Atom <| Symbol ">"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run10 =
            \_ -> Expect.equal
                  (serialize <| Gt "key" (Number 42.0))
                  (Expression [ Atom <| Symbol ">"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run11 =
            \_ -> Expect.equal
                  (serialize <| Gte "key" (Str "foo"))
                  (Expression [ Atom <| Symbol ">="
                              , Atom <| String "key"
                              , Atom <| String "foo"
                              ]
                  )

        run12 =
            \_ -> Expect.equal
                  (serialize <| Gte "key" (Number 42))
                  (Expression [ Atom <| Symbol ">="
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run13 =
            \_ -> Expect.equal
                  (serialize <| Gte "key" (Number 42.0))
                  (Expression [ Atom <| Symbol ">="
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run14 =
            \_ -> Expect.equal
                  (serialize <| Lt "key" (Str "foo"))
                  (Expression [ Atom <| Symbol "<"
                              , Atom <| String "key"
                              , Atom <| String "foo"
                              ]
                  )

        run15 =
            \_ -> Expect.equal
                  (serialize <| Lt "key" (Number 42))
                  (Expression [ Atom <| Symbol "<"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run16 =
            \_ -> Expect.equal
                  (serialize <| Lt "key" (Number 42.0))
                  (Expression [ Atom <| Symbol "<"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run17 =
            \_ -> Expect.equal
                  (serialize <| Lte "key" (Str "foo"))
                  (Expression [ Atom <| Symbol "<="
                              , Atom <| String "key"
                              , Atom <| String "foo"
                              ]
                  )

        run18 =
            \_ -> Expect.equal
                  (serialize <| Lte "key" (Number 42))
                  (Expression [ Atom <| Symbol "<="
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run19 =
            \_ -> Expect.equal
                  (serialize <| Lte "key" (Number 42.0))
                  (Expression [ Atom <| Symbol "<="
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

    in
    Test.concat
        [ test "tzaware" run1
        , test "not tzaware" run2
        , test "or (tzaware, byname)" run3
        , test "and (or (tzaware, formula), byname)" run4
        , test "metakey" run5
        , test "eq str str" run6
        , test "eq str int" run7
        , test "gt str str" run8
        , test "gt str int" run9
        , test "gt str float" run10
        , test "gte str str" run11
        , test "gte str int" run12
        , test "gte str float" run13
        , test "lt str str" run14
        , test "lt str int" run15
        , test "lt str float" run16
        , test "lte str str" run17
        , test "lte str int" run18
        , test "lte str float" run19
        ]
