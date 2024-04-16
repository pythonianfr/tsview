module QeditorSuite exposing
    ( testParse
    , testSerialize
    )

import Expect
import Test exposing (Test, test)

import Lisp exposing
    ( Atom(..)
    , Expr(..)
    )
import Filter exposing
    ( FilterNode(..)
    , Value(..)
    , fromlisp
    , parse
    , serialize
    )



testFromLispString : Test
testFromLispString =
    let
        run1 =
            \_ -> Expect.equal
                  ( Result.map serialize
                        <| fromlisp "(by.everything)"
                  )
                  (Ok <| Expression [ Atom <| String "by.everything" ])
    in
    Test.concat [ test "from lisp" run1 ]


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
                              , Expression [ Atom <| Symbol "by.tzaware" ]
                              , Expression [ Atom <| Symbol "by.name"
                                           , Atom <| String "foo"
                                           ]
                              ])

        run4 =
            \_ -> Expect.equal
                  (serialize <| And [ Or [ TzAware, Formula ] , ByName "foo" ])
                  (Expression
                       [ Atom (Symbol "by.and")
                       , Expression
                             [ Atom (Symbol "by.or")
                             , Expression
                                   [ Atom (Symbol "by.tzaware") ]
                             , Expression
                                   [ Atom (Symbol "by.formula") ]
                             ]
                       , Expression
                             [ Atom (Symbol "by.name")
                             , Atom (String "foo")
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

        run20 =
            \_ -> Expect.equal
                  (serialize <| BySource "remote")
                  (Expression [ Atom <| Symbol "by.source"
                              , Atom <| String "remote"
                              ]
                  )

        run21 =
            \_ -> Expect.equal
                  (serialize <| ByInternalMetaitem "key" (Str "foo"))
                  (Expression [ Atom <| Symbol "by.internalmetaitem"
                              , Atom <| String "key"
                              , Atom <| String "foo"
                              ]
                  )

        run22 =
            \_ -> Expect.equal
                  (serialize <| ByInternalMetaitem "key" (Number 42))
                  (Expression [ Atom <| Symbol "by.internalmetaitem"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run23 =
            \_ -> Expect.equal
                  (serialize <| ByInternalMetaitem "key" (Number 42.0))
                  (Expression [ Atom <| Symbol "by.internalmetaitem"
                              , Atom <| String "key"
                              , Atom <| Float 42.0
                              ]
                  )

        run24 =
            \_ -> Expect.equal
                  (serialize Everything)
                  (Expression [ Atom <| Symbol "by.everything" ])

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
        , test "bysource" run20
        , test "internalmetaitem str str" run21
        , test "internalmetaitem str int" run22
        , test "internalmetaitem str float" run23
        , test "everythinf" run24
        ]


testParse : Test
testParse =
    let
        run1 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.tzaware" ])
                  (Ok <| TzAware)

        run2 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.name"
                                       , Atom <| String "foo"
                                       ]
                  )
                  (Ok <| ByName "foo")

        run3 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.metakey"
                                       , Atom <| String "foo"
                                       ]
                  )
                  (Ok <| ByMetakey "foo")

        run4 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.formula" ])
                  (Ok <| Formula)

        run5 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.formulacontents"
                                       , Atom <| String "foo"
                                       ]
                  )
                  (Ok <| FormulaContents "foo")

        run6 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "="
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| Eq "foo" (Str "bar"))

        run7 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "="
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| Eq "foo" (Number 42.0))

        run8 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "="
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| Eq "foo" (Number 42.0))

        run9 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "<"
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| Lt "foo" (Str "bar"))

        run10 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "<"
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| Lt "foo" (Number 42.0))

        run11 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "<"
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| Lt "foo" (Number 42.0))

        run12 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "<="
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| Lte "foo" (Str "bar"))

        run13 =
            \_ -> Expect.equal
                  (parse  <| Expression [ Atom <| Symbol "<="
                                        , Atom <| String "foo"
                                        , Atom <| Int 42
                                        ]
                  )
                  (Ok <| Lte "foo" (Number 42.0))

        run14 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "<="
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| Lte "foo" (Number 42.0))

        run15 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">"
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| Gt "foo" (Str "bar"))

        run16 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">"
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| Gt "foo" (Number 42.0))

        run17 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">"
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| Gt "foo" (Number 42.0))

        run18 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">="
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| Gte "foo" (Str "bar"))

        run19 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">="
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| Gte "foo" (Number 42.0))

        run20 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol ">="
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| Gte "foo" (Number 42.0))

        run21 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.metaitem"
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| ByMetaITem "foo" (Str "bar"))

        run22 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.metaitem"
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| ByMetaITem "foo" (Number 42.0))

        run23 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.metaitem"
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| ByMetaITem "foo" (Number 42.0))

        run24 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.source"
                                       , Atom <| String "remote"
                                       ]
                  )
                  (Ok <| BySource "remote")

        run25 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.everything" ])
                  (Ok <| Everything)

        run26 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.internalmetaitem"
                                       , Atom <| String "foo"
                                       , Atom <| String "bar"
                                       ]
                  )
                  (Ok <| ByInternalMetaitem "foo" (Str "bar"))

        run27 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.internalmetaitem"
                                       , Atom <| String "foo"
                                       , Atom <| Int 42
                                       ]
                  )
                  (Ok <| ByInternalMetaitem "foo" (Number 42.0))

        run28 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.internalmetaitem"
                                       , Atom <| String "foo"
                                       , Atom <| Float 42.0
                                       ]
                  )
                  (Ok <| ByInternalMetaitem "foo" (Number 42.0))

        run29 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.not"
                                       , Expression [ Atom <| Symbol "by.tzaware" ]
                                       ]
                  )
                 (Ok <| Not TzAware )

        run30 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.not"
                                       , Expression [ Atom <| Symbol "by.and"
                                                    , Expression [ Atom <| Symbol "by.tzaware" ]
                                                    , Expression [ Atom <| Symbol "by.name"
                                                                 , Atom <| String "foo"
                                                                 ]
                                                    ]
                                       ]
                  )
                 (Ok <| Not <| And [ TzAware, ByName "foo" ])

        run31 =
            \_ -> Expect.equal
                  (parse <| Expression [ Atom <| Symbol "by.or"
                                       , Expression [ Atom <| Symbol "by.tzaware" ]
                                       , Expression [ Atom <| Symbol "by.name"
                                                    , Atom <| String "foo"
                                                    ]
                                       ]
                  )
                 (Ok <| Or [ TzAware, ByName "foo" ])
    in
    Test.concat
        [ test "parse tzaware" run1
        , test "parse byname" run2
        , test "parse bymetakey" run3
        , test "parse byformula" run4
        , test "parse byformulacontents" run5
        , test "parse eq str str" run6
        , test "parse eq str int" run7
        , test "parse eq str float" run8
        , test "parse lt str str" run9
        , test "parse lt str int" run10
        , test "parse lt str float" run11
        , test "parse lte str str" run12
        , test "parse lte str int" run13
        , test "parse lte str float" run14
        , test "parse gt str str" run15
        , test "parse gt str int" run16
        , test "parse gt str float" run17
        , test "parse gte str str" run18
        , test "parse gte str int" run19
        , test "parse gte str float" run20
        , test "parse metaitem str str" run21
        , test "parse metaitem str int" run22
        , test "parse metaitem str float" run23
        , test "parse bysource" run24
        , test "parse everything" run25
        , test "parse imetaitem str str" run26
        , test "parse imetaitem str int" run27
        , test "parse imetaitem str float" run28
        , test "parse not tzaware" run29
        , test "parse not and (tzaware byname)" run30
        , test "parse or (tzaware byname)" run31
        ]
