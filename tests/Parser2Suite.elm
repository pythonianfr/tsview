module Parser2Suite exposing (testParsing)

import Test
import Either

import Editor.SpecParser exposing (parserErrorsToString)
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)

import TestUtil exposing (T)
import JsonSpec exposing (gSpec, returnType)


formulaTests : List (T String)
formulaTests =
    [ T "Parsing void" """
(   )
    """ """
(2, 5) =>  Expecting: Valid operator name
    """

    , T "Parsing internal void" """
( + 2   ()  )
    """ """
(2, 10) =>  Expecting: Valid operator name
    """

    , T "+ OK" "( +   2.  6.7 )" "(+ 2 6.7)"

    , T "+ series OK" "( + 2   #:flag #t  #:b (+ 1 6))" """
(+
    2
    (+ 1 6)
    #:flag #t)
"""

    , T "**" """
(** (series "foo") 0.5)
    """ """
(**
    (series "foo")
    0.5)
"""

    , T "+ too many args" """
(+ 3 4 5)
    """ """
(2, 8) =>  Expecting Token: )
    """

    , T "@ unsupported op" """
(@ 3 4)
    """ """
(2, 2) =>  Expecting: Valid operator name
    """

    , T "+ wrong args" """
(+ a b 4)
    """ """
(2, 4) =>  Internal error: Lack of mandatory argument
    """

    , T "* Right" "(* -9.26e-08 #:b (+ 9.259e-02 -109))" """
(*
    -9.26e-8
    (+ 0.09259 -109))
"""

    , T "priority OK" "( priority  4 3  6 7 )" "(priority 4 3 6 7)"

    , T "priority series OK" """
( priority (* 4 9) (* 3 (+ 2 6)) ( + 7 1 #:flag #t))
""" """
(priority
    (* 4 9)
    (*
        3
        (+ 2 6))
    (+ 7 1 #:flag #t))
"""

    , T "ordering OK" "(+ #:flag  #t #:b 4  #:a 3  )" "(+ 3 4 #:flag #t)"

    , T "#flag duplicated" """
(+  3 4  #:flag  #t #:flag #f)
    """ """
(2, 30) =>  Internal error: Duplicate keyword for argument
"""

    , T "#k1, k2 OK" """
  (priority  3  4 10  #:k2  7.3 #:k1  "x" )
    """ """
(priority 3 4 10 #:k1 "x" #:k2 7.3)
"""

    , T "full OK" """
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
    """ """
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
        render : String -> String
        render input =
            parseFormula gSpec returnType input
                |> parserErrorsToString
                |> Either.unpack identity renderFormula
    in
    TestUtil.buildTests render formulaTests |> Test.concat
