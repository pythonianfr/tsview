module TsView.Formula.Spec.Render exposing
    ( renderSpec
    , strBaseType
    , strExpType
    , strInputType
    )

import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.Spec.Type as S


strInputType : S.InputType -> String
strInputType iType =
    case iType of
        S.Int ->
            "Int"

        S.Number ->
            "Number"

        S.String ->
            "String"

        S.Bool ->
            "Bool"

        S.Timestamp ->
            "Timestamp"

        S.SearchString ->
            "SearchString"


strBaseType : S.BaseType -> String
strBaseType bType =
    case bType of
        S.BaseInput x ->
            strInputType x

        S.Series ->
            "Series"


strExpType : S.ExpType -> String
strExpType eType =
    case eType of
        S.ExpBaseType x ->
            strBaseType x

        S.SList x ->
            "List[" ++ strExpType x ++ "]"

        S.Union xs ->
            let
                typesToStr =
                    NE.map strExpType >> NE.toList >> String.join ", "
            in
            "Union[" ++ typesToStr xs ++ "]"


renderSection : String -> (a -> String) -> List a -> List ( Int, String )
renderSection title rdrType =
    NE.fromList
        >> Maybe.map (NE.map (rdrType >> Tuple.pair 3) >> NE.cons ( 2, title ))
        >> Maybe.map NE.toList
        >> Maybe.withDefault []


renderOperator : S.Operator -> List ( Int, String )
renderOperator op =
    let
        rdrKArg ( k, x ) =
            k ++ ": " ++ strExpType x
    in
    List.concat
        [ List.singleton ( 1, op.name )
        , renderSection "arguments:" strExpType op.args
        , renderSection "keyword_arguments:" rdrKArg op.kargs
        , List.singleton ( 2, "return: " ++ strExpType op.return )
        ]


render : ( S.BaseType, NE.Nonempty S.Operator ) -> List ( Int, String )
render ( baseType, ops ) =
    NE.foldl
        (\op xs -> xs ++ renderOperator op)
        [ ( 0, strBaseType baseType ) ]
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
