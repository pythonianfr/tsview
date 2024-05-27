module Editor.UI.Render exposing (renderEditor)

import Bool.Extra as Bool
import Maybe.Extra as Maybe

import String.Format as F
import List.Nonempty as NE
import RoseTree.Tree as Tree

import Optics.Core as O exposing (o)
import OpticsExtra as OE

import Editor.Type as T
import Editor.SpecRender as SpecRender

import Editor.UI.Type exposing (..)


type alias Indent =
    { indent : Int
    , isHead : Bool
    , rowType : String
    , rowStr : String
    }

rowType_ : O.SimpleLens ls Indent String
rowType_ = O.lens .rowType <| \s a -> { s | rowType = a }


renderArg : String -> Maybe String -> Maybe String
renderArg x = Maybe.map (\v -> x ++ "=" ++ v)

renderArgs : List (Maybe String) -> String
renderArgs xs = Maybe.values xs |> String.join " "

renderUnion :  Union -> String
renderUnion {primitiveTypes, primitiveType} = "Union[{{ }}]({{ }})"
    |> F.value (SpecRender.renderPrimitiveTypes primitiveTypes)
    |> F.value (SpecRender.renderPrimitiveType primitiveType)

renderSelector : Selector -> String
renderSelector {returnType} = "Selector[{{ }}]"
    |> F.value (SpecRender.renderReturnType returnType)

renderOperator : T.Operator -> String
renderOperator op =
    "Operator({{ }} => {{ }})"
        |> F.value op.name
        |> F.value (SpecRender.renderReturnType op.return)

renderInput : Input -> String
renderInput {literalType, value, userInput, errMess} =
    "Input[{{ }}]({{ }})"
        |> F.value (SpecRender.renderLiteralType literalType)
        |> F.value (renderArgs
        [ renderArg "value" <| Maybe.map SpecRender.renderLiteralExpr value
        , renderArg "userInput" userInput
        , renderArg "errMess" errMess
        ])

renderNode : Node -> String
renderNode node = case node of
    Primitive (PInput x) -> renderInput x

    Primitive (POperator op) -> renderOperator op

    VarArgs (T.Packed t) -> "CVarArgs({{ }})"
        |> F.value (SpecRender.renderPrimitiveType t)

    Packed (T.Packed t) op -> "CPacked({{ }}, {{ }})"
        |> F.value (SpecRender.renderPrimitiveType t)
        |> F.value (renderOperator op)

renderEntry : Entry Node -> List String
renderEntry {union, selector, entryType} = Maybe.values
    [ Maybe.map renderUnion union
    , Maybe.map renderSelector selector
    , Just <| renderNode entryType
    ]

renderRowType : RowType -> (String, List String)
renderRowType rowType = case rowType of
    RArg {key, entry} ->
        ( "RArg({{ }})"
            |> F.value key
        , renderEntry entry
        )

    ROptArgs ->
        ( "ROptArgs"
        , [ "OperatorOptions" ]
        )

    ROptArg {key, defaultValue, entry} ->
        let
            v = Maybe.unwrap "None" SpecRender.renderLiteralExpr defaultValue
        in
        ( "ROptArg({{ }}, Default={{ }})"
            |> F.value key
            |> F.value v
        , renderEntry entry
        )

    RVarItem entry ->
        ( "RVarItem"
        , renderEntry <| O.over entryType_ Primitive entry
        )

    RVarEnd ->
        ( "RVarEnd"
        , [ "AddItem" ]
        )

renderEditionRow : Int -> EditionRow -> List Indent
renderEditionRow i {rowType, isExpand} =
    let
        extra =
            [ renderArg "isExpand" (Maybe.map Bool.toString isExpand)
            ]
    in renderRowType rowType |> \(t, rows) -> rows
        |> O.over OE.listHead_ (\x -> renderArgs (Just x :: extra))
        |> O.over OE.consLast_ (\(x, xs) ->
            ( x
            , List.map (\y -> y ++ ",") xs
            )
        )
        |> O.over OE.cons_ (\(x, xs) ->
            ( Indent i True t x
            , List.map (Indent i False t) xs
            )
        )

renderTree : Int -> EditionTree -> List Indent
renderTree i tree =
    List.append
        (renderEditionRow i <| O.get OE.node_ tree)
        (List.concatMap (renderTree (i + 1)) <| O.get OE.forest_ tree)

renderIndent : Indent -> String
renderIndent {indent, isHead, rowType, rowStr} =
    let
        tab = String.repeat 2 " "
        doIndent i = String.repeat i tab

        prefix = if String.isEmpty rowType then "" else rowType ++ ": "
        shift = String.repeat (String.length prefix) " "

    in (doIndent indent) ++ (if isHead then prefix else shift) ++ rowStr

renderEditor : Editor -> String
renderEditor {root} =
    O.review treeRoot_ root
        |> renderTree 0
        |> (\xs -> List.append xs [Indent 0 False "" ""])
        |> List.map renderIndent
        |> String.join "\n"
