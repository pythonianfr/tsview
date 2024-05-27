module Editor.UI.Render exposing (renderEditionTree)

import Bool.Extra as Bool
import Maybe.Extra as Maybe
import Basics.Extra exposing (uncurry)

import Tree
import List.Extra
import String.Format as F
import List.NonEmpty as NE

import Editor.Type as T
import Editor.SpecRender as SpecRender

import Editor.UI.Type exposing (..)


type alias Indent =
    { indent : Int
    , isHead : Bool
    , rowType : String
    , rowStr : String
    }

renderArg : String -> Maybe String -> Maybe String
renderArg x = Maybe.map (\v -> x ++ "=" ++ v)

renderArgs : List (Maybe String) -> String
renderArgs xs = Maybe.values xs |> String.join " "

renderOperator : T.Operator -> String
renderOperator op = "Operator({{ }})" |> F.value op.name

renderInput : Input -> String
renderInput {literalType, value, userInput, errMess} =
    "Input[{{ }}]({{ }})"
        |> F.value (SpecRender.renderLiteralType literalType)
        |> F.value (renderArgs
        [ renderArg "value" <| Maybe.map SpecRender.renderEditableValue value
        , renderArg "userInput" userInput
        , renderArg "errMess" errMess
        ])

renderArgType : ArgType -> T.Key -> String
renderArgType t k = case t of
    Arg -> "Arg({{ }})" |> F.value k

    OptArg v -> "OptArg({{ }}, Default={{ }})"
        |> F.value k
        |> F.value (SpecRender.renderEditableValue v)

renderUnion :  Union -> String
renderUnion {specTypes, specType} = "Union[{{ }}]({{ }})"
    |> F.value (SpecRender.renderSpecTypes specTypes)
    |> F.value (SpecRender.renderSpecType specType)

renderSelector : Selector -> String
renderSelector {specType} = "Selector[{{ }}]"
    |> F.value (SpecRender.renderSpecType specType)


renderRawEntry : (a -> String) -> RawEntry a -> NE.NonEmpty String
renderRawEntry f {argType, argKey, unions, selector, entryType} =
    let
        renderUnions : List Union -> List (Maybe String)
        renderUnions = List.map (renderUnion >> Just)
    in
    List.append
        (Maybe.unwrap [] (NE.toList >> renderUnions) unions)
        [ Maybe.map renderSelector selector
        , Just <| f entryType
        ]
    |> Maybe.values
    |> NE.fromCons (renderArgType argType argKey)

renderEntryType : EntryType -> String
renderEntryType t = case t of
    InputEntry x -> renderInput x
    OperatorEntry op -> renderOperator op

renderEntry : Entry -> NE.NonEmpty String
renderEntry = renderRawEntry renderEntryType

renderVarArgEntry : VarArgEntry -> NE.NonEmpty String
renderVarArgEntry = renderRawEntry
    (\t -> "VarArgEntry[{{ }}]" |> F.value (SpecRender.renderSpecType t))

renderRowType : RowType -> (String, NE.NonEmpty String)
renderRowType r = case r of
    TopRow op -> ("Top", NE.singleton <| renderOperator op)

    EntryRow e -> ("EntryRow", renderEntry e)

    OptionsRow -> ("OperatorOptions", NE.singleton "OptArgs")

    VarArgsRow (VarArg e) -> ("VarArgsRow", renderVarArgEntry e)

    VarArgsRow (VarItem e) -> ("VarItem", renderEntry e)

    VarArgsRow VarEnd -> ("VarEnd", NE.singleton "AddItem")

renderEditionRow : Int -> EditionRow -> List Indent
renderEditionRow i {rowType, isExpand} =
    let
        extra =
            [ renderArg "isExpand" (Maybe.map Bool.toString isExpand)
            ]
    in renderRowType rowType |> uncurry (\t (headLine, tail) ->
        (renderArgs (Just headLine :: extra)) :: tail
        |> List.Extra.unconsLast
        |> Maybe.map (\(last, xs) -> List.append
            (List.map (\x -> x ++ ",") xs)
            (List.singleton last))
        |> Maybe.andThen List.Extra.uncons
        |> Maybe.map (\(x, xs) ->
            (Indent i True t x) :: (List.map (Indent i False t) xs))
        |> Maybe.withDefault [])

renderTree : Int -> EditionTree -> List Indent
renderTree i tree =
    let
        node = Tree.label tree
        forest = Tree.children tree
    in List.append
        (renderEditionRow i node)
        (List.concatMap (renderTree (i + 1)) forest)

renderIndent : Indent -> String
renderIndent {indent, isHead, rowType, rowStr} =
    let
        tab = String.repeat 2 " "
        doIndent i = String.repeat i tab

        prefix = if String.isEmpty rowType then "" else rowType ++ ": "
        shift = String.repeat (String.length prefix) " "

    in (doIndent indent) ++ (if isHead then prefix else shift) ++ rowStr

renderEditionTree : EditionTree -> String
renderEditionTree tree =
    renderTree 0 tree
        |> (\xs -> List.append xs [Indent 0 False "" ""])
        |> List.map renderIndent
        |> String.join "\n"

