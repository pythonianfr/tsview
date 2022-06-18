module TshLispSuite exposing (testParsing)

import Either
import Expect
import Test exposing (Test, test)
import TshLisp.Parser exposing (parse)
import TshLisp.Render exposing (render)


type alias T =
    { name : String
    , input : String
    , output : String
    }


formulaTests : List T
formulaTests =
    [ T "+ OK" "( +   2.  6.7 )" "(+ 2 6.7)"
    , T "+ series OK" "( + 2   #:flag #t  #:b (+ 1 6))" """
(+
    2
    #:flag #t
    #:b (+ 1 6))
"""
    , T "+ many args" " ( +  3  4  5 ) " "(+ 3 4 5)"
    , T "@ unsupported op" "(@ 3 4)" "FAILED"
    , T "+ wrong args" "(+ ab 4)" "FAILED"
    , T "* Right" "(* -9.26e-08 #:b (+ 9.259e-02 -109))" """
(*
    -9.26e-8
    #:b (+ 0.09259 -109))
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
    , T "#flag #t OK" "(+  #:b 4 #:a 3  #:flag  #t)" "(+ #:b 4 #:a 3 #:flag #t)"
    , T
        "#flag duplicated"
        "(+  3 4  #:flag  nil  #:flag #f)"
        "(+ 3 4 #:flag nil #:flag #f)"
    , T "#k1, k2 OK" " (priority  3  4 10  nil  #:k2  7.3 #:k1  \"x\" )" """
(priority 3 4 10 nil #:k2 7.3 #:k1 "x")
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
    (series "gaz.nl" #:weight 3 #:fill "all")
    (*
        1.2
        (series #:name "gaz.fr"))
    (series #:fill 79 #:name "gaz.de")
    #:k1 4.7)
    """
    ]


testParsing : Test
testParsing =
    let
        parseAndRender : String -> String
        parseAndRender =
            parse >> Either.unpack (always "FAILED") render
    in
    List.map
        (\x ->
            let
                res =
                    Expect.equal
                        (parseAndRender x.input)
                        (String.trim x.output)
            in
            test x.name (always res)
        )
        formulaTests
        |> Test.concat
