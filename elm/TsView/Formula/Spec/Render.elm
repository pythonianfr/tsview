module TsView.Formula.Spec.Render exposing (renderSpec)

import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (boolToString)


renderSection : String -> (a -> String) -> List a -> List ( Int, String )
renderSection title rdrType =
    NE.fromList
        >> Maybe.map (NE.map (rdrType >> Tuple.pair 3) >> NE.cons ( 2, title ))
        >> Maybe.map NE.toList
        >> Maybe.withDefault []


renderOperator : S.Operator -> List ( Int, String )
renderOperator op =
    let
        rdrDft v =
            case v of
                S.BoolValue x ->
                    boolToString x

                S.IntValue x ->
                    String.fromInt x

                S.NumberValue x ->
                    String.fromFloat x

                S.StringValue x ->
                    "\"" ++ x ++ "\""

                S.TimestampValue x ->
                    x

                _ ->
                    ""

        rdrKArg ( k, x, v ) =
            let
                dft =
                    case v of
                        S.Empty ->
                            ""

                        _ ->
                            " Default=" ++ rdrDft v
            in
            k ++ ": " ++ S.strExpType x ++ dft
    in
    List.concat
        [ List.singleton ( 1, op.name )
        , renderSection "arguments:" S.strExpType (List.map Tuple.second op.args)
        , renderSection "keyword_arguments:" rdrKArg op.kargs
        , List.singleton ( 2, "return: " ++ S.strExpType op.return )
        ]


render : ( S.BaseType, NE.Nonempty S.Operator ) -> List ( Int, String )
render ( baseType, ops ) =
    NE.foldl
        (\op xs -> xs ++ renderOperator op)
        [ ( 0, S.strBaseType baseType ) ]
        ops


renderSpec : S.Spec -> String
renderSpec =
    let
        sep =
            [ ( 0, String.repeat 5 "-" ) ]

        iStr =
            String.repeat 4 " "
    in
    S.specToList
        >> List.foldl (\x xs -> xs ++ render x ++ sep) sep
        >> List.map (\( i, s ) -> String.repeat i iStr ++ s)
        >> String.join "\n"
