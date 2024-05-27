module EditorSuite exposing (mainTest)


import Test
import Expect

import Either

import Editor.Type as ET
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)
import Editor.UI.Type exposing
    (initializeTypedExpr
    , buildEditionTree
    , renderTypedExpr
    , parseEditionTree
    )
import Editor.UI.Render exposing (renderEditionTree)

import JsonSpec exposing (spec, gSpec)
import TestUtil exposing (T)


initTests : List (T ET.SpecType)
initTests =
    [ T "init Packed" (ET.Packed ET.Series) "()"
    ]

testInitialize : Test.Test
testInitialize =
    let
        render x = initializeTypedExpr x |> renderFormula
    in
    Test.describe "testInitialize" <| TestUtil.buildTests render initTests

buildEditionTreeTests : List (T String)
buildEditionTreeTests =
    [ T "Small tree"  "(* 3 4)" """
Top: Operator(*) isExpand=True
  EntryRow: Arg(a),
            Selector[Number],
            Input[Number](value=3)
  EntryRow: Arg(b),
            Union[Series, Number](Number),
            Selector[Number],
            Input[Number](value=4)
"""
    , T "priority" """
(priority
    (series "UK")
    (series "FR")
    (series "DE")
)
""" """
Top: Operator(priority) isExpand=True
  VarArgsRow: Arg(serieslist),
              VarArgEntry[Union[Series, Number]]
    VarItem: Arg(list_item) isExpand=True,
             Union[Series, Number](Series),
             Selector[Series],
             Operator(series)
      EntryRow: Arg(name),
                Input[SearchString](value="UK")
      OperatorOptions: OptArgs isExpand=False
        EntryRow: OptArg(fill, Default=nil),
                  Union[String, Int](String),
                  Input[String](value=nil)
        EntryRow: OptArg(weight, Default=nil),
                  Selector[Number],
                  Input[Number](value=nil)
    VarItem: Arg(list_item) isExpand=True,
             Union[Series, Number](Series),
             Selector[Series],
             Operator(series)
      EntryRow: Arg(name),
                Input[SearchString](value="FR")
      OperatorOptions: OptArgs isExpand=False
        EntryRow: OptArg(fill, Default=nil),
                  Union[String, Int](String),
                  Input[String](value=nil)
        EntryRow: OptArg(weight, Default=nil),
                  Selector[Number],
                  Input[Number](value=nil)
    VarItem: Arg(list_item) isExpand=True,
             Union[Series, Number](Series),
             Selector[Series],
             Operator(series)
      EntryRow: Arg(name),
                Input[SearchString](value="DE")
      OperatorOptions: OptArgs isExpand=False
        EntryRow: OptArg(fill, Default=nil),
                  Union[String, Int](String),
                  Input[String](value=nil)
        EntryRow: OptArg(weight, Default=nil),
                  Selector[Number],
                  Input[Number](value=nil)
  VarEnd: AddItem
  OperatorOptions: OptArgs isExpand=False
    EntryRow: OptArg(k1, Default=nil),
              Union[String, Number](String),
              Input[String](value=nil)
    EntryRow: OptArg(k2, Default=nil),
              Union[Number, Timestamp](Number),
              Selector[Number],
              Input[Number](value=nil)
"""
    ]

testBuildEditionTree : Test.Test
testBuildEditionTree =
    let
        render x = parseFormula spec x
            |> Either.map (buildEditionTree gSpec)
            |> Either.unpack identity renderEditionTree
    in
    Test.describe "buildEditionTree"
        <| TestUtil.buildTests render buildEditionTreeTests

testRenderTypedExpr : Test.Test
testRenderTypedExpr =
    let
        render x = parseFormula spec x
            |> Either.map (buildEditionTree gSpec)
            -- Actual test
            |> Either.map (renderTypedExpr >> buildEditionTree gSpec)
            |> Either.unpack identity renderEditionTree
    in
    Test.describe "renderTypedExpr"
        <| TestUtil.buildTests render buildEditionTreeTests

testParseEditionTree : Test.Test
testParseEditionTree =
    let
        render x = parseFormula spec x
            |> Either.map (buildEditionTree gSpec)
            -- Actual test
            |> Either.andThen (parseEditionTree gSpec
                >> (\(mErr, tree) -> Either.leftFromMaybe tree mErr)
            )
            |> Either.unpack identity renderEditionTree
    in
    Test.describe "parseEditionTree"
        <| TestUtil.buildTests render buildEditionTreeTests

mainTest : Test.Test
mainTest = Test.concat
    [ testInitialize
    , testBuildEditionTree
    , testRenderTypedExpr
    , testParseEditionTree
    ]
