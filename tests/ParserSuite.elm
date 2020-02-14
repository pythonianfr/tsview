module ParserSuite exposing (parsing, testStringParser)

import Either exposing (Either(..))
import Expect
import Lazy.Tree.Zipper exposing (fromTree)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import Test exposing (Test, concat, test)
import TsView.Formula.Parser as P exposing (parseSpec, stringParser)
import TsView.Formula.Renderer exposing (renderString)
import TsView.Formula.Spec as S


spec : S.Spec
spec =
    NE.Nonempty
        (S.Operator
            "series"
            [ S.Arg S.SearchString ]
            (S.OptArgs
                [ S.OptArg "fill" <| S.Union (NE.Nonempty S.String [ S.Int ])
                , S.OptArg "weight" S.Float
                ]
            )
        )
        [ S.Operator
            "+"
            [ S.Arg S.Float
            , S.Arg <| S.Union (NE.Nonempty S.Float [ S.Series ])
            ]
            (S.OptArgs [ S.OptArg "a" S.Int ])
        , S.Operator
            "*"
            [ S.Arg S.Float
            , S.Arg <| S.Union (NE.Nonempty S.Series [ S.Float ])
            ]
            (S.OptArgs [])
        , S.Operator
            "priority"
            [ S.Arg <| S.SList <| S.Union (NE.Nonempty S.Series [ S.Float ])
            ]
            (S.OptArgs
                [ S.OptArg "k1" <| S.Union (NE.Nonempty S.String [ S.Float ])
                , S.OptArg "k2" <| S.Union (NE.Nonempty S.Float [ S.Date ])
                ]
            )
        ]


render : String -> Either String String
render =
    parseSpec spec
        >> Either.andThen (fromTree >> renderString >> Right)


type alias T =
    { name : String
    , input : String
    , output : Either String String
    }


formulaTests : List T
formulaTests =
    [ T "+ Right" "( +   2.  6.7 )" <| Right "(+ 2 6.7)"
    , T "+ Right series" "( +   2  (+ 1 6) )" <| Right """
(+
    2
    (+ 1 6))
"""
    , T "+ too many args" "(+ 3 4 5)" <| Left "FAILED"
    , T "@ unsupported op" "(@ 3 4)" <| Left "FAILED"
    , T "+ wrong args" "(+ ab 4)" <| Left "FAILED"
    , T "* Right" "(* -9.26e-08 (+ 9.259e-02 -109))" <| Right """
(*
    -9.26e-8
    (+ 0.09259 -109))
"""
    , T "priority Right" "( priority  4 3  6 7 )" <| Right "(priority 4 3 6 7)"
    , T "priority Right series" """
( priority  (* 4 9) (* 3 (+ 2 6)) ( + 7 1 #:a 8))
""" <| Right """
(priority
    (* 4 9)
    (*
        3
        (+ 2 6))
    (+ 7 1 #:a 8))
"""
    , T "#a:7 Right" "(+  3 4  #:a  7)" <| Right "(+ 3 4 #:a 7)"
    , T "#a duplicated" "(+  3 4  #:a  7 #:a 5)" <| Left "FAILED"
    , T "#k1, k2" " (priority  3  4 10  #:k2  7.3 #:k1  \"x\" )" <| Right """
(priority 3 4 10 #:k1 "x" #:k2 7.3)
"""
    , T "full" """
(priority
    (+  3 (series "a.b" #:weight 3.5 )  )
    (priority
        (* 10.3 (series "gaz.es"))
        (+ -1.7 (series "gaz.pt" #:weight .3) #:a 12)
        (series "gaz.es.pt.predicted" #:fill 2 #:weight 1.1)
        #:k2 "2020-01-24"
    )
    ( series "gaz.nl" #:weight 3. #:fill "all" )
    ( * 1.2 (series "gaz.fr" ) )
    (series "gaz.de"  #:fill  79 )
    #:k1 4.7
)
    """ <| Right """
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
            #:a 12)
        (series "gaz.es.pt.predicted" #:fill 2 #:weight 1.1)
        #:k2 "2020-01-24")
    (series "gaz.nl" #:fill "all" #:weight 3)
    (*
        1.2
        (series "gaz.fr"))
    (series "gaz.de" #:fill 79)
    #:k1 4.7)
    """
    ]


parsing : Test
parsing =
    let
        f x =
            Expect.equal (render x.input) (Either.map String.trim x.output)
    in
    List.map (\x -> test x.name (\_ -> f x)) formulaTests |> concat


testStringParser : Test
testStringParser =
    let
        input =
            """
"serie-2020"
"""

        inputParser : Parser String
        inputParser =
            Parser.succeed identity
                |. Parser.spaces
                |= stringParser
                |. Parser.spaces
                |. Parser.end

        run i r =
            \_ -> Expect.equal (Parser.run inputParser i) (Ok r)
    in
    concat
        [ test "stringParser multi" (run input "serie-2020")
        , test "stringParser single" (run "\"s1.x\"" "s1.x")
        ]
