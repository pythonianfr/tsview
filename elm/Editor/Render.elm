module Editor.Render exposing
    ( renderFormula
    , renderBool
    , renderLiteralExpr
    , inspectTypedOperator
    )

import Either
import Maybe.Extra as Maybe

import Optics.Core as O exposing (o)

import AssocList as Assoc
import OpticsExtra as OE

import Editor.Type as T
import Editor.SpecRender as SpecRender


type alias IsInline = Bool

type RenderValue
    = Value (Maybe T.LiteralExpr)
    | Operator IsInline String (List ArgType)

type ArgType
    = Arg RenderValue
    | OptArg T.Key RenderValue


-- invalid Prism but useful for testing
renderValue_ : O.SimplePrism pr ArgType RenderValue
renderValue_ = O.prism Arg <| \argType ->
    case argType of
        Arg x -> Either.Right x

        OptArg _ x -> Either.Right x

value_ : O.SimplePrism pr RenderValue (Maybe T.LiteralExpr)
value_ = O.prism Value <| \s ->
    case s of
        Value a -> Either.Right a

        _ -> Either.Left s


renderBool : Bool -> String
renderBool bool = case bool of
    True -> "#t"

    False -> "#f"

renderLiteralExpr : T.LiteralExpr -> String
renderLiteralExpr literalExpr = case literalExpr of
    T.BoolExpr x -> renderBool x

    _ -> SpecRender.renderLiteralExpr literalExpr

renderPrimitiveExpr : T.PrimitiveExpr -> RenderValue
renderPrimitiveExpr primitiveExpr = case primitiveExpr of
    T.LiteralExpr _ x -> Value x

    T.OperatorExpr x -> renderTypedOperator x

renderArgExpr : T.ArgExpr -> List RenderValue
renderArgExpr argExpr = case argExpr of
    T.PrimitiveExpr x ->
        List.singleton <| renderPrimitiveExpr x
    
    T.UnionExpr _ (_, x) ->
        List.singleton <| renderPrimitiveExpr x

    T.VarArgsExpr _ xs ->
        List.map renderPrimitiveExpr xs

    T.PackedExpr _ x ->
        List.singleton <| renderTypedOperator x

renderTypedOperator : T.TypedOperator -> RenderValue
renderTypedOperator {operator, typedArgs, typedOptArgs} =
    let
        args = List.concatMap
            (\x -> renderArgExpr x |> List.map Arg)
            (Assoc.values typedArgs)

        optArgs = List.concatMap
            (\(k, x) -> renderArgExpr x |>  List.map (OptArg k))
            (Assoc.toList typedOptArgs)

        isInline = List.all (O.is (o renderValue_ value_)) (args ++ optArgs) 

    in Operator isInline operator.name (args ++ optArgs)


type alias Indent =
    { indent : Int
    , line : String
    }

line_ : O.SimpleLens ls Indent String
line_ = O.lens .line (\s a -> { s | line = a })

renderArg : Int -> ArgType -> List Indent
renderArg i argType = case argType of
    Arg x -> renderValue i x

    OptArg k x -> renderValue i x
        |> O.over (o OE.listHead_ line_) (\s -> "#:" ++ k ++ " " ++ s)

renderValue : Int -> RenderValue -> List Indent
renderValue i v = case v of
    Value x ->
        [ Indent i <| Maybe.unwrap " " renderLiteralExpr x ]

    Operator False name args ->
        (Indent i name) :: (List.concatMap (renderArg (i + 1)) args)
            |> O.over (o OE.listHead_ line_) (\s -> "(" ++ s)
            |> O.over (o OE.listLast_ line_) (\s -> s ++ ")")

    Operator True name [] ->
        [ Indent i <| "(" ++ name ++ ")" ]
        
    Operator True name args ->
        let sargs = List.concatMap (renderArg i) args
                |> List.map .line
                |> String.join " "
        in
        [ Indent i <| "(" ++ name ++ " " ++ sargs ++ ")" ]

renderFormula : T.TypedOperator -> T.FormulaCode
renderFormula typedOperator =
    let
        tab = String.repeat 4 " "

        renderIndent : Indent -> String
        renderIndent {indent, line} = String.repeat indent tab ++ line

    in renderTypedOperator typedOperator
        |> renderValue 0
        |> List.map renderIndent
        |> String.join "\n"

inspectTypedOperator : T.TypedOperator -> String
inspectTypedOperator typedOperator = "TODO : render TypedOperator"
