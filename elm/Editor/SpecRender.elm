module Editor.SpecRender exposing 
    ( renderLiteralType
    , renderSpecType
    , renderSpecTypes
    , renderEditableValue
    , renderSpec
    )

import Bool.Extra as Bool

import AssocList as Assoc
import List.NonEmpty as NE

import Editor.Type as T exposing (..)


renderLiteralType : T.LiteralType -> String
renderLiteralType t = case t of
    T.Int -> "Int"

    T.Number -> "Number"

    T.String -> "String"

    T.Bool -> "Bool"

    T.TimestampString -> "Timestamp"

    T.SearchString -> "SearchString"

renderSpecType : T.SpecType -> String
renderSpecType sType = case sType of
    T.Editable x -> renderLiteralType x

    T.Timestamp -> "Timestamp"

    T.Query -> "Query"

    T.Series -> "Series"

    T.VarArgs x -> "List[" ++ renderSpecType x ++ "]"

    T.Packed x -> "Packed[" ++ renderSpecType x ++ "]"

    T.Union xs -> "Union[" ++ renderSpecTypes xs ++ "]"

renderSpecTypes : NE.NonEmpty SpecType -> String
renderSpecTypes xs =
    NE.map renderSpecType xs |> NE.toList |> String.join ", "

renderEditableValue : EditableValue -> String
renderEditableValue v = case v of
    T.Nil -> "nil"

    T.BoolValue x -> Bool.toString x

    T.IntValue x -> String.fromInt x

    T.NumberValue x -> String.fromFloat x

    T.StringValue x -> "\"" ++ x ++ "\""

    T.TimestampValue x -> x

renderSection : String -> (a -> String) -> T.KAssoc a -> List ( Int, String )
renderSection title rdrType ks =
    let
        rdrArg ( k, v ) = ( 2, k ++ ": " ++ rdrType v )
    in
    Assoc.toList ks
        |> List.map rdrArg
        |> NE.fromList
        |> Maybe.map (NE.cons ( 1, title ) >> NE.toList)
        |> Maybe.withDefault []


renderOperator : T.Operator -> List ( Int, String )
renderOperator op =
    let

        rdrArg v = renderSpecType v

        rdrOptArg ( x, v ) =
            renderSpecType x ++ " Default=" ++ renderEditableValue v
    in
    List.concat
        [ List.singleton ( 0, op.name )
        , renderSection "arguments:" rdrArg op.args
        , renderSection "optional_arguments:" rdrOptArg op.optArgs
        , List.singleton ( 1, "return: " ++ renderSpecType op.return )
        ]


renderSpec : T.Spec -> String
renderSpec =
    let
        sep = [ ( 0, String.repeat 5 "-" ) ]
        iStr = String.repeat 4 " "
    in
    Assoc.values
        >> List.foldl (\x xs -> xs ++ renderOperator x ++ sep) sep
        >> List.map (\( i, s ) -> String.repeat i iStr ++ s)
        >> String.join "\n"
