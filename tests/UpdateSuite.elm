module UpdateSuite exposing (testUpdate)

import Either exposing (Either)
import Expect
import Test exposing (Test, test)
import Tree.Zipper exposing (toTree)
import TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)
import TsView.Formula.EditionTree.Parser exposing (parseFormula)
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.EditionTree.Update exposing (updateZipper)
import TsView.Formula.Spec.Parser exposing (parseSpecString)
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (Path, foldEither, withPath)


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
    "priority2",
    [
      [
        "return",
        "Series"
      ],
      [
        "serieslist",
        "List[Union[Number, Series]]"
      ]
    ]
  ]
]
"""


type alias Input =
    { path : Path
    , action : ET.EditAction
    }


type alias T =
    { name : String
    , formula : String
    , inputs : List Input
    , result : String
    }


tests : List T
tests =
    [ T
        "+ 2. -> 9"
        "( +   2.  6.7 )"
        [ Input (List.repeat 5 0) (ET.ReadInput "9") ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : +
      Argument
        Input operator selector : Number
          ExpType : Number = 9
      Argument
        ExpType : Union[Number, Series]
          Input operator selector : Number
            ExpType : Number = 6.7

    """
    , T
        "ReturnTypes Number -> Series"
        "( +   2.  6.7 )"
        [ Input [] (ET.ReadInput "Series")
        , Input [ 0, 0, 0, 0 ] (ET.ReadInput "FR")
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : series
      Argument
        ExpType : SearchString = "FR"
      Options :
        OptionalArgument fill
          ExpType : Union[String, Int]
            ExpType : String
        OptionalArgument weight
          Input operator selector : Number
            ExpType : Number
    """
    , T
        "+ -> *"
        "(+ 1 2)"
        [ Input [ 0 ] (ET.ReadInput "*")
        , Input (List.repeat 5 0) (ET.ReadInput "-1.2e-3")
        , Input ([ 0, 0, 1 ] ++ List.repeat 5 0) (ET.ReadInput "test,a,b")
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : *
      Argument
        Input operator selector : Number
          ExpType : Number = -0.0012
      Argument
        ExpType : Union[Series, Number]
          Operator selector : Series
            Operator : series
              Argument
                ExpType : SearchString = "test,a,b"
              Options :
                OptionalArgument fill
                  ExpType : Union[String, Int]
                    ExpType : String
                OptionalArgument weight
                  Input operator selector : Number
                    ExpType : Number
    """
    , T
        "Input Number -> +"
        "(+ 7 3.4)"
        [ Input (List.repeat 4 0) (ET.ReadInput "+")
        , Input (List.repeat 8 0) (ET.ReadInput "6.7")
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : +
      Argument
        Input operator selector : Number
          Operator : +
            Argument
              Input operator selector : Number
                ExpType : Number = 6.7
            Argument
              ExpType : Union[Number, Series]
                Input operator selector : Number
                  ExpType : Number
      Argument
        ExpType : Union[Number, Series]
          Input operator selector : Number
            ExpType : Number = 3.4

    """
    , T
        "+ -> Number"
        "(+ (+ 3 7) 2)"
        [ Input (List.repeat 4 0) (ET.ReadInput "Number")
        , Input (List.repeat 5 0) (ET.ReadInput "5.8")
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : +
      Argument
        Input operator selector : Number
          ExpType : Number = 5.8
      Argument
        ExpType : Union[Number, Series]
          Input operator selector : Number
            ExpType : Number = 2
    """
    , T
        "+ Number -> Series"
        "( +   2.  6.7 )"
        [ Input [ 0, 0, 1, 0 ] (ET.ReadInput "Series")
        , Input ([ 0, 0, 1, 0 ] ++ List.repeat 4 0) (ET.ReadInput "uk")
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : +
      Argument
        Input operator selector : Number
          ExpType : Number = 2
      Argument
        ExpType : Union[Number, Series]
          Operator selector : Series
            Operator : series
              Argument
                ExpType : SearchString = "uk"
              Options :
                OptionalArgument fill
                  ExpType : Union[String, Int]
                    ExpType : String
                OptionalArgument weight
                  Input operator selector : Number
                    ExpType : Number
    """
    , T
        "List Remove / Add"
        """
(priority2 4 1.2 (series "uk") 9 2)
    """
        (let
            listPath =
                [ 0, 0, 0, 0 ]

            itemPath i =
                listPath ++ [ i ]
         in
         [ Input listPath ET.ListAdd
         , Input (itemPath 3) ET.ListRemove
         , Input listPath ET.ListAdd
         , Input (itemPath 1) ET.ListRemove
         , Input (listPath ++ [ 3, 0, 0 ]) (ET.ReadInput "7")
         ]
        )
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : priority2
      Argument
        ExpType : List[Union[Number, Series]]
          ExpType : Union[Number, Series]
            Input operator selector : Number
              ExpType : Number = 4
          ExpType : Union[Number, Series]
            Operator selector : Series
              Operator : series
                Argument
                  ExpType : SearchString = "uk"
                Options :
                  OptionalArgument fill
                    ExpType : Union[String, Int]
                      ExpType : String
                  OptionalArgument weight
                    Input operator selector : Number
                      ExpType : Number
          ExpType : Union[Number, Series]
            Input operator selector : Number
              ExpType : Number = 2
          ExpType : Union[Number, Series]
            Input operator selector : Number
              ExpType : Number = 7
          ExpType : Union[Number, Series]
            Input operator selector : Number
              ExpType : Number
    """
    , T
        "Complete Remove and Add"
        """
(priority2 (series "uk"))
    """
        [ Input [ 0, 0, 0, 0, 0 ] ET.ListRemove
        , Input [ 0, 0, 0, 0 ] ET.ListAdd
        ]
        """
ReturnTypes : Number, Series
  Operator selector : Series
    Operator : priority2
      Argument
        ExpType : List[Union[Number, Series]]
          ExpType : Union[Number, Series]
            Input operator selector : Number
              ExpType : Number
    """
    ]


applyInputs : List Input -> ( S.Spec, ET.EditionTree ) -> Either String ET.EditionTree
applyInputs inputs ( spec, parsedTree ) =
    let
        doUpdate : Input -> ET.EditionTree -> Either String ET.EditionTree
        doUpdate input tree =
            withPath input.path tree <|
                \zipper ->
                    updateZipper spec input.action zipper |> toTree |> Either.singleton
    in
    foldEither doUpdate parsedTree inputs


testUpdate : Test
testUpdate =
    let
        specEither =
            parseSpecString jsonSpec |> Either.voidLeft "spec parsing failed"

        runTest : T -> Expect.Expectation
        runTest t =
            specEither
                |> Either.andThen
                    (\spec ->
                        parseFormula spec t.formula
                            |> Either.map (Tuple.pair spec)
                    )
                |> Either.andThen (applyInputs t.inputs)
                |> Either.unpack
                    Expect.fail
                    (inspectEditionTree >> Expect.equal (String.trim t.result))
    in
    List.map (\t -> test t.name (\_ -> runTest t)) tests |> Test.concat
