module TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)

import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)
import Tree exposing (Tree)
import TsView.Formula.EditionTree.Type as T exposing (EditionNode)
import TsView.Formula.Spec.Render as SR


inspectType : Int -> T.EditionType -> Maybe ( Int, String )
inspectType i t =
    let
        noIndent x =
            Just ( i, x )

        indent x =
            Just ( i + 1, x )
    in
    case t of
        T.ReturnTypeT xs ->
            let
                vals =
                    NE.toList xs
                        |> List.map SR.strBaseType
                        |> String.join ", "
            in
            indent <| "ReturnTypes : " ++ vals

        T.SelectorT x ->
            indent <| "Operator selector : " ++ SR.strBaseType x

        T.InputSelectorT x ->
            indent <| "Input operator selector : " ++ SR.strInputType x

        T.OperatorT (T.Operator name _ _) ->
            indent <| "Operator : " ++ name

        T.OptArgsT T.NoOptArgs ->
            Nothing

        T.OptArgsT _ ->
            indent "Options :"

        T.ArgT x ->
            indent "Argument"

        T.OptArgT (T.OptArg name _) ->
            indent <| "OptionalArgument " ++ name

        T.ArgTypeT (T.ArgType x) ->
            indent <| "ExpType : " ++ SR.strExpType x


valueToString : T.Value -> Maybe String
valueToString value =
    case value of
        T.Empty ->
            Nothing

        T.BoolValue True ->
            Just "True"

        T.BoolValue False ->
            Just "False"

        T.IntValue x ->
            Just <| String.fromInt x

        T.NumberValue x ->
            Just <| String.fromFloat x

        T.StringValue x ->
            Just <| "\"" ++ x ++ "\""

        T.TimestampValue x ->
            Just x


inspectValue : String -> T.Value -> String
inspectValue line =
    valueToString
        >> Maybe.map (\x -> line ++ " = " ++ x)
        >> Maybe.withDefault line


inspectNode : Int -> EditionNode -> Maybe ( Int, String )
inspectNode i node =
    inspectType i node.editionType
        |> Maybe.map
            (Tuple.mapSecond
                (\s ->
                    Tuple.second node.input
                        |> Either.map (inspectValue s)
                        |> Either.fromRight s
                )
            )


inspect : Int -> Tree EditionNode -> List ( Int, String )
inspect iParent tree =
    let
        ( iChild, head ) =
            case inspectNode iParent (Tree.label tree) of
                Just ( i, s ) ->
                    ( i, [ ( iParent, s ) ] )

                Nothing ->
                    ( iParent, [] )
    in
    head ++ List.concatMap (inspect iChild) (Tree.children tree)


inspectEditionTree : Tree EditionNode -> String
inspectEditionTree tree =
    let
        iStr =
            String.repeat 2 " "
    in
    inspect 0 tree
        |> List.map (\( i, s ) -> String.repeat i iStr ++ s)
        |> String.join "\n"
