module EditorSuite exposing (mainTest)


import Test
import Expect

import Either

import Editor.Type as ET
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)
import Editor.UI.Type exposing
    ( buildEditor
    , renderTypedOperator
    , parseEditor
    )
import Editor.UI.Render exposing (renderEditor)

import JsonSpec exposing (gSpec, returnType)
import TestUtil exposing (T)


buildEditorTests : List (T String)
buildEditorTests =
    [ T "Small tree"  "(* 3 4)" """
RArg(EDITOR): Selector[Series],
              Operator(* => Series)
  RArg(a): Selector[Number],
           Input[Number](value=3)
  RArg(b): Union[Series, Number](Number),
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
RArg(EDITOR): Selector[Series],
              Operator(priority => Series)
  RArg(serieslist): Selector[Packed[Series, Number]] isExpand=True,
                    CVarArgs(Union[Series, Number])
    RVarItem: Union[Series, Number](Series) isExpand=True,
              Selector[Series],
              Operator(series => Series)
      RArg(name): Input[SearchString](value="UK")
      ROptArgs: OperatorOptions isExpand=False
        ROptArg(fill, Default=None): Union[String, Int](String),
                                     Input[String]()
        ROptArg(weight, Default=None): Selector[Number],
                                       Input[Number]()
    RVarItem: Union[Series, Number](Series) isExpand=True,
              Selector[Series],
              Operator(series => Series)
      RArg(name): Input[SearchString](value="FR")
      ROptArgs: OperatorOptions isExpand=False
        ROptArg(fill, Default=None): Union[String, Int](String),
                                     Input[String]()
        ROptArg(weight, Default=None): Selector[Number],
                                       Input[Number]()
    RVarItem: Union[Series, Number](Series) isExpand=True,
              Selector[Series],
              Operator(series => Series)
      RArg(name): Input[SearchString](value="DE")
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
        render x = buildEditor gSpec returnType x
            |> parseEditor
            |> renderEditor
    in
    Test.describe "buildEditor"
        <| TestUtil.buildTests render buildEditorTests

testRenderTypedOperator : Test.Test
testRenderTypedOperator =
    let
        render x = buildEditor gSpec returnType x
            |> renderTypedOperator
            |> renderEditor
    in
    Test.describe "renderTypedOperator"
        <| TestUtil.buildTests render buildEditorTests

testParseEditor : Test.Test
testParseEditor =
    let
        render x = buildEditor gSpec returnType x
            |> parseEditor
            |> renderEditor
    in
    Test.describe "parseEditor"
        <| TestUtil.buildTests render buildEditorTests

mainTest : Test.Test
mainTest = Test.concat
    [ testBuildEditor
    , testRenderTypedOperator
    , testParseEditor
    ]
