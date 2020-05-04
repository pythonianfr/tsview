module TsView.Formula.EditionTree.Inspect exposing
    ( inspectEditionTree
    , logEditionTree
    )

import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)
import Tree exposing (Tree)
import TsView.Formula.EditionTree.Type as ET exposing (EditionNode)
import TsView.Formula.Spec.Type as S


logEditionTree : String -> Tree EditionNode -> Tree EditionNode
logEditionTree mess tree =
    let
        dummyLog a b =
            --Debug.log a b
            ()

        _ =
            dummyLog
                (String.join "\n"
                    [ ""
                    , "=> " ++ mess
                    , inspectEditionTree tree
                    , ""
                    ]
                )
                "____"
    in
    tree


inspectType : Int -> ET.EditionType -> Maybe ( Int, String )
inspectType i t =
    let
        noIndent x =
            Just ( i, x )

        indent x =
            Just ( i + 1, x )
    in
    case t of
        ET.ReturnTypeT xs ->
            let
                vals =
                    NE.toList xs
                        |> List.map S.strBaseType
                        |> String.join ", "
            in
            indent <| "ReturnTypes : " ++ vals

        ET.SelectorT x ->
            indent <| "Operator selector : " ++ S.strBaseType x

        ET.InputSelectorT x ->
            indent <| "Input operator selector : " ++ S.strInputType x

        ET.OperatorT (ET.Operator name _ _) ->
            indent <| "Operator : " ++ name

        ET.OptArgsT ET.NoOptArgs ->
            Nothing

        ET.OptArgsT _ ->
            indent "Options :"

        ET.ArgT x ->
            indent "Argument"

        ET.OptArgT (ET.OptArg name _ _) ->
            indent <| "OptionalArgument " ++ name

        ET.ExpTypeT x ->
            indent <| "ExpType : " ++ S.strExpType x


valueToString : S.Value -> Maybe String
valueToString value =
    case value of
        S.Empty ->
            Nothing

        S.BoolValue True ->
            Just "Srue"

        S.BoolValue False ->
            Just "False"

        S.IntValue x ->
            Just <| String.fromInt x

        S.NumberValue x ->
            Just <| String.fromFloat x

        S.StringValue x ->
            Just <| "\"" ++ x ++ "\""

        S.TimestampValue x ->
            Just x


inspectValue : String -> S.Value -> String
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
                        |> Either.unpack identity identity
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
