module Editor.SpecRender exposing
    ( renderPrimitiveType
    , renderPrimitiveTypes
    , renderArgType
    , renderReturnType
    , renderLiteralType
    , renderLiteralExpr
    , renderSpec
    , findOperators
    , findOperator
    )

import Bool.Extra as Bool
import Maybe.Extra as Maybe
import Either exposing (Either)

import List.Nonempty as NE

import AssocList as Assoc

import Editor.Type as T


-- Spec basic operations
findOperators : T.Spec -> T.ReturnType -> Either String T.Operators
findOperators spec returnType =
    Assoc.get returnType spec
        |> Either.fromMaybe
            ("No operator for type " ++ renderReturnType returnType)

findOperator : T.Spec -> T.ReturnType -> T.Key -> Either String T.Operator
findOperator spec returnType key =
    findOperators spec returnType |> Either.andThen
        (Assoc.get key >> Either.fromMaybe ("No operator for " ++ key))


-- basic rendering
renderLiteralType : T.LiteralType -> String
renderLiteralType t = case t of
    T.Int -> "Int"

    T.Number -> "Number"

    T.String -> "String"

    T.Bool -> "Bool"

    T.TimestampString -> "Timestamp"

    T.SeriesName -> "SeriesName"

renderOperatorOutputType : T.OperatorOutputType -> String
renderOperatorOutputType (T.OperatorOutputType s) = s

renderPrimitiveType : T.PrimitiveType -> String
renderPrimitiveType primitiveType = case primitiveType of
    T.Literal t -> renderLiteralType t

    T.OperatorOutput t -> renderOperatorOutputType t

renderReturnType : T.ReturnType -> String
renderReturnType returnType = case returnType of
    T.ReturnPrimitiveType t ->
        renderPrimitiveType t

    T.ReturnList t ->
        "List[" ++ renderPrimitiveType t ++ "]"

renderPrimitiveTypes : NE.Nonempty T.PrimitiveType -> String
renderPrimitiveTypes xs =
    NE.map renderPrimitiveType xs |> NE.toList |> String.join ", "

renderArgType : T.ArgType -> String
renderArgType argType = case argType of
    T.PrimitiveType t ->
        renderPrimitiveType t

    T.UnionType xs ->
        "Union[" ++ renderPrimitiveTypes xs ++ "]"

    T.PackedType (T.Packed t) ->
        "Packed[" ++ renderPrimitiveType t ++ "]"

renderLiteralExpr : T.LiteralExpr -> String
renderLiteralExpr literalExpr = case literalExpr of
    T.BoolExpr x -> Bool.toString x

    T.IntExpr x -> String.fromInt x

    T.NumberExpr x -> String.fromFloat x

    T.StringExpr x -> "\"" ++ x ++ "\""

    T.TimestampExpr x -> x


type alias Indent =
    { indent : Int
    , line : String
    }


renderSection : String -> (a -> String) -> T.KAssoc a -> List Indent
renderSection title rdrType ks =
    let rdrArg ( k, v ) = Indent 2 (k ++ ": " ++ rdrType v)
    in Assoc.toList ks
        |> List.map rdrArg
        |> NE.fromList
        |> Maybe.unwrap
            []
            (\xs -> NE.cons (Indent 1 title) xs |> NE.toList)

renderOperator : T.Operator -> List Indent
renderOperator op =
    let
        rdrArg = renderArgType

        rdrDefault = Maybe.unwrap "None" renderLiteralExpr

        rdrOptArg ( x, v ) =
            renderArgType x ++ " Default=" ++ rdrDefault v

        return = "return: " ++ renderReturnType op.return
    in
    List.concat
        [ List.singleton <| Indent 0 op.name
        , renderSection "arguments:" rdrArg op.args
        , renderSection "optional_arguments:" rdrOptArg op.optArgs
        , List.singleton <| Indent 1 return
        ]

render : (a -> List Indent) -> List a -> String
render f xs =
    let
        sep : Indent
        sep = Indent 0 (String.repeat 77 "-" )

        tab : String
        tab = String.repeat 4 " "

        renderIndent : Indent -> String
        renderIndent {indent, line} = String.repeat indent tab ++ line

    in List.concatMap (f >> List.append [sep]) xs
        |> (\ys -> if (List.isEmpty ys) then [] else (List.append ys [sep]))
        |> List.map renderIndent
        |> String.join "\n"

renderOperators : (T.ReturnType, T.Operators) -> List Indent
renderOperators (returnType, operators) =
    (Indent 0 "") ::
    (Indent 1 <| renderReturnType returnType) ::
    (Indent 0 "") ::
    (List.concatMap renderOperator <| Assoc.values operators)

renderSpec : T.Spec -> String
renderSpec spec = Assoc.toList spec |> render renderOperators
