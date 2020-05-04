module ViewSuite exposing (testView)

import Either
import Expect
import Test exposing (Test, test)
import Tree.Zipper exposing (fromTree)
import TsView.Formula.EditionTree.Parser exposing (parseFormula)
import TsView.Formula.EditionTree.View exposing (renderRowTree, strRowTree)
import TsView.Formula.Spec.Parser exposing (parseSpecString)


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
        "List[Number]"
      ]
    ]
  ]
]
"""


type alias T =
    { name : String
    , formula : String
    , result : String
    }


tests : List T
tests =
    [ T
        "(+ 2. 6.7)"
        "( +   2.  6.7 )"
        """
Row ReturnType : ReturnTypeT[Number, Series], SelectorT[Series], OperatorT[+]
  Row Arg : ArgT, InputSelectorT[Number], ExpTypeT[Number]
  Row Arg : ArgT, ExpTypeT[Union[Number, Series]], InputSelectorT[Number], ExpTypeT[Number]
    """
    , T
        "priority"
        "(priority 7 1.2  3.6)"
        """
Row ReturnType : ReturnTypeT[Number, Series], SelectorT[Series], OperatorT[priority]
  Row Arg : ArgT, ExpTypeT[List[Number]]
  Row SListItem : InputSelectorT[Number], ExpTypeT[Number]
  Row SListItem : InputSelectorT[Number], ExpTypeT[Number]
  Row SListItem : InputSelectorT[Number], ExpTypeT[Number]
  Row SListAdd
    """
    ]


testView : Test
testView =
    let
        specEither =
            parseSpecString jsonSpec |> Either.voidLeft "spec parsing failed"

        runTest : T -> Expect.Expectation
        runTest t =
            specEither
                |> Either.andThen
                    (\spec ->
                        parseFormula spec t.formula
                    )
                |> Either.map (fromTree >> renderRowTree)
                |> Either.unpack
                    Expect.fail
                    (strRowTree >> Expect.equal (String.trim t.result))
    in
    List.map (\t -> test t.name (\_ -> runTest t)) tests |> Test.concat
