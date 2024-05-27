module Parser2Suite exposing (testParsing)

import Test
import Either

import ParserExtra exposing (parserErrorsToString)

import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)

import TestUtil exposing (T)
import JsonSpec exposing (spec)


formulaTests : List (T (String, String))
formulaTests =
    [ T "Parsing void"
    ( "Series"
    , """
(   )
    """
    ) """
(2, 5) =>  Expecting: Valid operator name
    """

    , T "Parsing internal void"
    ( "Number"
    , """
( + 2   ()  )
    """
    ) """
(2, 9) =>  Invalid: Lack of a Number mandatory argument
    """

    , T "+ OK"
    ( "Number"
    , "( +   2.  6.7 )"
    )
    "(+ 2 6.7)"

    , T "+ series OK"
    ( "Number"
    , "( + 2   #:flag #t  #:b (+ 1 6))"
    ) """
(+
    2
    (+ 1 6)
    #:flag #t)
"""

    , T "**"
    ( "Series"
    , """
(** (series "foo") 0.5)
    """
    ) """
(**
    (series "foo")
    0.5)
"""

    , T "+ too many args"
    ( "Number"
    , """
(+ 3 4 5)
    """
    ) """
(2, 8) =>  Expecting Token: )
    """

    , T "@ unsupported op"
    ( "Number"
    , """
(@ 3 4)
    """
    ) """
(2, 2) =>  Expecting: Valid operator name
    """

    , T "+ wrong args"
    ( "Number"
    , """
(+ a b 4)
    """
    ) """
(2, 4) =>  Invalid: Lack of a Number mandatory argument
    """

    , T "* Right"
    ( "Number"
    , "(* -9.26e-08 #:b (+ 9.259e-02 -109))"
    ) """
(*
    -9.26e-8
    (+ 0.09259 -109))
"""

    , T "ipriority OK"
    ( "Number"
    , "( ipriority  4 3  6 7 )"
    ) "(ipriority 4 3 6 7)"

    , T "ipriority operators OK"
    ( "Number"
    , """
( ipriority (* 4 9) (* 3 (+ 2 6)) ( + 7 1 #:flag #t))
""" ) """
(ipriority
    (* 4 9)
    (*
        3
        (+ 2 6))
    (+ 7 1 #:flag #t))
"""

    , T "ordering OK"
    ( "Number"
    , "(+ #:flag  #t #:b 4  #:a 3  )"
    ) "(+ 3 4 #:flag #t)"

    , T "#flag duplicated"
    ( "Number"
    , """
(+  3 4  #:flag  #t #:flag #f)
    """
    ) """
(2, 30) =>  Invalid: Duplicate keyword for argument
"""

    , T "#k1, k2 OK"
    ( "Number"
    , """
  (ipriority  3  4 10  #:k2  7.3 #:k1  "x" )
    """
    ) """
(ipriority 3 4 10 #:k1 "x" #:k2 7.3)
"""

    , T "findseries"
    ( "Series"
    , """
(add (findseries (by.name "pmax_avail.nuclear.be")))
"""
    ) """
(add
    (findseries
        (by.name "pmax_avail.nuclear.be")))
"""

    , T "No operator in a list"
    ( "Series"
    , """
(priority (series "a") (series "b") () ())
    """
    ) """
(2, 38) =>  Expecting: Valid operator name
    """

    , T "full OK"
    ( "Series"
    , """
(priority
    (+  3 (series "a.b" #:weight 3.5 )  )
    (priority
        (* 10.3 (series "gaz.es"))
        (+ -1.7 (series "gaz.pt" #:weight .3) #:flag  #t)
        (series "gaz.es.pt.predicted" #:fill 2 #:weight 1.1)
        #:k2 (timedelta (today) #:years 5)
    )
    ( series "gaz.nl" #:weight 3. #:fill "all" )
    ( * 1.2 (series #:name  "gaz.fr" ) )
    (series  #:fill  79 #:name  "gaz.de" )
    #:k1 4.7
)
    """
    ) """
(priority
    (+
        3
        (series "a.b" #:weight 3.5))
    (priority
        (*
            10.3
            (series "gaz.es"))
        (+
            -1.7
            (series "gaz.pt" #:weight 0.3)
            #:flag #t)
        (series "gaz.es.pt.predicted" #:fill 2 #:weight 1.1)
        #:k2 (timedelta
            (today)
            #:years 5))
    (series "gaz.nl" #:fill "all" #:weight 3)
    (*
        1.2
        (series "gaz.fr"))
    (series "gaz.de" #:fill 79)
    #:k1 4.7)
    """
    ]

testParsing : Test.Test
testParsing =
    let
        render : (String, String) -> String
        render (returnTypeStr, input) =
            parseFormula spec returnTypeStr input
                |> parserErrorsToString
                |> Either.unpack identity renderFormula
    in
    TestUtil.buildTests render formulaTests |> Test.concat
