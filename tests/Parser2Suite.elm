module Parser2Suite exposing (testParsing)

import Either
import Expect
import Test exposing (Test, test)
import TsView.Formula.Parser exposing (parseFormula)
import TsView.Formula.Render exposing (renderString)
import TsView.Formula.Spec.Parser2 exposing (parseSpecString)


type alias T =
    { name : String
    , input : String
    , output : String
    }


jsonSpec : String
jsonSpec =
    """
[
  [
    "series",
    [
      [
        "return",
        "Series"
      ],
      [
        "name",
        "seriesname"
      ],
      [
        "fill",
        "Default[Union[str, int]=None]"
      ],
      [
        "weight",
        "Default[Number=None]"
      ]
    ]
  ],
  [
    "+",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "a",
        "Number"
      ],
      [
        "b",
        "Union[Number, Series]"
      ],
      [
        "flag",
        "Default[bool=False]"
      ]
    ]
  ],
  [
    "*",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "a",
        "Number"
      ],
      [
        "b",
        "Union[Series, Number]"
      ]
    ]
  ],
  [
    "priority",
    [
      [
        "return",
        "Series"
      ],
      [
        "serieslist",
        "List[Union[Series, Number]]"
      ],
      [
        "k1",
        "Default[Union[str, Number]=None]"
      ],
      [
        "k2",
        "Default[Union[Number, Timestamp]=None]"
      ]
    ]
  ],
  [
    "timedelta",
    [
      [
        "return",
        "Timestamp"
      ],
      [
        "date",
        "Timestamp"
      ],
      [
        "years",
        "Default[int=0]"
      ],
      [
        "months",
        "Default[int=0]"
      ],
      [
        "weeks",
        "Default[int=0]"
      ],
      [
        "days",
        "Default[int=0]"
      ],
      [
        "hours",
        "Default[int=0]"
      ],
      [
        "minutes",
        "Default[int=0]"
      ]
    ]
  ],
  [
    "today",
    [
      [
        "return",
        "Timestamp"
      ],
      [
        "naive",
        "Default[bool=False]"
      ],
      [
        "tz",
        "Default[str=None]"
      ]
    ]
  ]
]
"""


formulaTests : List T
formulaTests =
    [ T "+ OK" "( +   2.  6.7 )" "(+ 2 6.7)"
    , T "+ series OK" "( + 2   #:flag #t  #:b (+ 1 6))" """
(+
    2
    (+ 1 6)
    #:flag #t)
"""
    , T "+ too many args" "(+ 3 4 5)" "FAILED"
    , T "@ unsupported op" "(@ 3 4)" "FAILED"
    , T "+ wrong args" "(+ ab 4)" "FAILED"
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
    , T "#flag duplicated" "(+  3 4  #:flag  #t #:flag #f)" "FAILED"
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


testParsing : Test
testParsing =
    let
        specEither =
            parseSpecString jsonSpec |> Either.voidLeft "spec parsing failed"

        render : String -> String
        render input =
            specEither
                |> Either.andThen (\spec -> parseFormula spec input)
                |> Either.unpack (always "FAILED") renderString
    in
    List.map
        (\x ->
            let
                res =
                    Expect.equal (render x.input) (String.trim x.output)
            in
            test x.name (always res)
        )
        formulaTests
        |> Test.concat