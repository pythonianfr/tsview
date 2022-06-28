module TsView.Formula.Spec.Render2 exposing (renderSpec)

import AssocList as Assoc
import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.Type as T
import TsView.Formula.Utils2 as U


renderSection : String -> (a -> String) -> T.KAssoc a -> List ( Int, String )
renderSection title rdrType ks =
    let
        rdrArg ( k, v ) =
            ( 2, k ++ ": " ++ rdrType v )
    in
    Assoc.toList ks
        |> List.map rdrArg
        |> NE.fromList
        |> Maybe.map (NE.cons ( 1, title ) >> NE.toList)
        |> Maybe.withDefault []


renderOperator : T.Operator -> List ( Int, String )
renderOperator op =
    let
        rdrDft v =
            case v of
                T.NIL ->
                    "NIL"

                T.BoolValue x ->
                    U.boolToString x

                T.IntValue x ->
                    String.fromInt x

                T.NumberValue x ->
                    String.fromFloat x

                T.StringValue x ->
                    "\"" ++ x ++ "\""

                T.TimestampValue x ->
                    x

        rdrArg v =
            U.strSpecType v

        rdrOptArg ( x, v ) =
            U.strSpecType x ++ " Default=" ++ rdrDft v
    in
    List.concat
        [ List.singleton ( 0, op.name )
        , renderSection "arguments:" rdrArg op.args
        , renderSection "optional_arguments:" rdrOptArg op.optArgs
        , List.singleton ( 1, "return: " ++ U.strSpecType op.return )
        ]


renderSpec : T.Spec -> String
renderSpec =
    let
        sep =
            [ ( 0, String.repeat 5 "-" ) ]

        iStr =
            String.repeat 4 " "
    in
    Assoc.values
        >> List.foldl (\x xs -> xs ++ renderOperator x ++ sep) sep
        >> List.map (\( i, s ) -> String.repeat i iStr ++ s)
        >> String.join "\n"
