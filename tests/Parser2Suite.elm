module Parser2Suite exposing (testParsing)

import Either
import Test

import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderString)
import Editor.SpecParser exposing (parseSpecString)

import TestUtil exposing (T)
import JsonSpec exposing (jsonSpec)


formulaTests : List T
formulaTests =
    [ T "Parsing void" "(   )" "()"
    , T "Parsing internal void" "( + 2   ()  )" "(+ 2 ())"
    , T "+ OK" "( +   2.  6.7 )" "(+ 2 6.7)"
    , T "+ series OK" "( + 2   #:flag #t  #:b (+ 1 6))" """
(+
    2
    (+ 1 6)
    #:flag #t)
"""
    , T "**" "(** (series \"foo\") 0.5)" """
(**
    (series \"foo\")
    0.5)
"""
    , T "+ too many args" "(+ 3 4 5)" "ExpectingSymbol ) at row:1 col:8"
    , T "@ unsupported op" "(@ 3 4)" "ExpectingSymbol ) at row:1 col:2"
    , T "+ wrong args" "(+ ab 4)" "ProblemString Lack of mandatory argument at row:1 col:4"
    , T "* Right" "(* -9.26e-08 #:b (+ 9.259e-02 -109))" """
(*
    -9.26e-8
    (+ 0.09259 -109))
"""
    , T "priority OK" "( priority  4 3  6 7 )" "(priority 4 3 6 7)"
    , T "priority series OK" """
( priority  (* 4 9) (* 3 (+ 2 6)) ( + 7 1 #:flag #t))
""" """
(priority
    (* 4 9)
    (*
        3
        (+ 2 6))
    (+ 7 1 #:flag #t))
"""
    , T "#flag #t OK" "(+  #:b 4 #:a 3  #:flag  #t)" "(+ 3 4 #:flag #t)"
    , T "#flag duplicated" "(+  3 4  #:flag  #t #:flag #f)" "ProblemString Duplicate keyword for argument at row:1 col:30 ExpectingKeyword #:flagat row:1 col:30"
    , T "#k1, k2 OK" " (priority  3  4 10  #:k2  7.3 #:k1  \"x\" )" """
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
        parsedspec =
            parseSpecString jsonSpec |> Either.unpack Tuple.first identity

        render : String -> String
        render input =
            case parseFormula parsedspec input |> Either.toResult of
                Ok parsedformula -> renderString parsedformula
                Err err -> String.trim err
    in
    TestUtil.buildTests render formulaTests |> Test.concat
