module EditorSuite exposing (mainTest)


import Test
import Expect

import Either

import Editor.Type as ET
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)
import Editor.UI.Type exposing
    ( buildEditor
    , initEditor
    , testTypedOperator
    , parseEditor
    )
import Editor.UI.Render exposing (renderEditor)

import JsonSpec exposing (spec)
import TestUtil exposing (T)


buildEditorTests : List (T (String, String))
buildEditorTests =
    [ T "Small tree"  ("Number", "(* 3 4)") """
RArg(EDITOR): Selector[Number],
              Operator(* => Number)
  RArg(a): Selector[Number],
           Input[Number](value=3)
  RArg(b): Selector[Number],
           Input[Number](value=4)
"""
    , T "priority"
    ( "Series"
    , """
(priority
    (series "UK")
    (series "FR")
    (series "DE")
)
"""
    ) """
RArg(EDITOR): Selector[Series],
              Operator(priority => Series)
  RArg(serieslist): Selector[List[Series]] isExpand=True,
                    CVarArgs(Series)
    RVarItem: Selector[Series] isExpand=True,
              Operator(series => Series)
      RArg(name): Input[SeriesName](value="UK")
      ROptArgs: OperatorOptions isExpand=False
        ROptArg(fill, Default=None): Union[String, Int](String),
                                     Input[String]()
        ROptArg(weight, Default=None): Selector[Number],
                                       Input[Number]()
    RVarItem: Selector[Series] isExpand=True,
              Operator(series => Series)
      RArg(name): Input[SeriesName](value="FR")
      ROptArgs: OperatorOptions isExpand=False
        ROptArg(fill, Default=None): Union[String, Int](String),
                                     Input[String]()
        ROptArg(weight, Default=None): Selector[Number],
                                       Input[Number]()
    RVarItem: Selector[Series] isExpand=True,
              Operator(series => Series)
      RArg(name): Input[SeriesName](value="DE")
      ROptArgs: OperatorOptions isExpand=False
        ROptArg(fill, Default=None): Union[String, Int](String),
                                     Input[String]()
        ROptArg(weight, Default=None): Selector[Number],
                                       Input[Number]()
    RVarEnd: AddItem
  ROptArgs: OperatorOptions isExpand=False
    ROptArg(k1, Default=None): Union[String, Number](String),
                               Input[String]()
    ROptArg(k2, Default=None): Union[Number, Timestamp](Number),
                               Selector[Number],
                               Input[Number]()
"""
    ]

testBuildEditor : Test.Test
testBuildEditor =
    let
        render (returnType, x) = buildEditor spec returnType x
            |> renderEditor
    in
    Test.describe "buildEditor"
        <| TestUtil.buildTests render buildEditorTests

testRenderTypedOperator : Test.Test
testRenderTypedOperator =
    let
        render (returnType, x) = buildEditor spec returnType x
            |> testTypedOperator
            |> renderEditor
    in
    Test.describe "testTypedOperator"
        <| TestUtil.buildTests render buildEditorTests

testParseEditor : Test.Test
testParseEditor =
    let
        render (returnType, x) = buildEditor spec returnType x
            |> parseEditor
            |> renderEditor
    in
    Test.describe "parseEditor"
        <| TestUtil.buildTests render buildEditorTests

initEditorTests : List (T String)
initEditorTests =
    [ T "Series init" "Series" """
RArg(EDITOR): Selector[Series],
              Operator( => Series)
    """
    , T "Number init" "Number" """
RArg(EDITOR): Operator( => Number)
"""
    ]

testInitEditor : Test.Test
testInitEditor =
    let
        render returnType =
            initEditor spec returnType |> renderEditor
    in
    Test.describe "initEditor"
        <| TestUtil.buildTests render initEditorTests

mainTest : Test.Test
mainTest = Test.concat
    [ testBuildEditor
    , testRenderTypedOperator
    , testParseEditor
    , testInitEditor
    ]
