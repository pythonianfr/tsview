module Editor.SpecRender exposing
    ( renderPrimitiveType
    , renderPrimitiveTypes
    , renderSpecType
    , renderLiteralExpr
    , renderSpec
    , renderGSpec
    )

import Bool.Extra as Bool
import Maybe.Extra as Maybe

import AssocList as Assoc
import List.Nonempty as NE

import Editor.Type as T


renderPrimitiveType : T.PrimitiveType -> String
renderPrimitiveType primitiveType = case primitiveType of
    T.Literal t -> T.renderLiteralType t

    T.OperatorOutput t -> T.renderOperatorOutputType t

    T.Union xs -> "Union[" ++ renderPrimitiveTypes xs ++ "]"

renderPrimitiveTypes : NE.Nonempty T.PrimitiveType -> String
renderPrimitiveTypes xs =
    NE.map renderPrimitiveType xs |> NE.toList |> String.join ", "

renderCompositeType : T.CompositeType -> String
renderCompositeType compositeType = case compositeType of
    T.VarArgs t -> "List[" ++ renderPrimitiveType t ++ "]"

    T.Packed t -> "Packed[" ++ renderPrimitiveType t ++ "]"

renderSpecType : T.SpecType -> String
renderSpecType sType = case sType of
    T.PrimitiveType t -> renderPrimitiveType t

    T.CompositeType t -> renderCompositeType t

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
        rdrArg = renderSpecType

        rdrDefault = Maybe.unwrap "None" renderLiteralExpr

        rdrOptArg ( x, v ) =
            renderSpecType x ++ " Default=" ++ rdrDefault v

        return = "return: " ++ renderSpecType op.return
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
        sep = Indent 0 (String.repeat 5 "-" )

        tab : String
        tab = String.repeat 4 " "

        renderIndent : Indent -> String
        renderIndent {indent, line} = String.repeat indent tab ++ line

    in List.concatMap (f >> List.append [sep]) xs
        |> (\ys -> if (List.isEmpty ys) then [] else (List.append ys [sep]))
        |> List.map renderIndent
        |> String.join "\n"

renderSpec : T.Spec -> String
renderSpec spec = Assoc.values spec |> render renderOperator

renderOperators : (T.BaseReturnType, T.Operators) -> List Indent
renderOperators (baseReturnType, operators) =
    (Indent 1 <| T.renderBaseReturnType baseReturnType) ::
    (Indent 0 "") ::
    (List.concatMap renderOperator <| Assoc.values operators)

renderGSpec : T.GSpec -> String
renderGSpec {gOperators} = Assoc.toList gOperators |> render renderOperators
