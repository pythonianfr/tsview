module TsView.Formula.EditionTree.Render exposing (renderString)

import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)
import Tree exposing (Tree)
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.Spec.Type as S


type alias RenderNode =
    { isBottom : Bool
    , node : ET.EditionNode
    }


nil : String
nil =
    "nil"


quoted : String -> String
quoted x =
    "\"" ++ x ++ "\""


valueToString : S.Value -> String
valueToString value =
    case value of
        S.Empty ->
            nil

        S.BoolValue True ->
            "#t"

        S.BoolValue False ->
            "#f"

        S.IntValue x ->
            String.fromInt x

        S.NumberValue x ->
            String.fromFloat x

        S.StringValue x ->
            quoted x

        S.TimestampValue x ->
            quoted x


buildRenderTree : Tree ET.EditionNode -> Tree RenderNode
buildRenderTree tree =
    let
        n =
            Tree.label tree

        forest =
            Tree.children tree
                |> List.map buildRenderTree

        isNotOperator r =
            case r.node.editionType of
                ET.OperatorT _ ->
                    False

                _ ->
                    True

        isBottom =
            forest
                |> List.map Tree.label
                |> List.all (\r -> r.isBottom && isNotOperator r)
    in
    Tree.tree (RenderNode isBottom n) forest


renderTree : Int -> Tree RenderNode -> List ( Int, String )
renderTree indent tree =
    let
        r =
            Tree.label tree

        n =
            r.node

        renderInput : String
        renderInput =
            Either.unpack
                (\_ -> Tuple.first n.input |> quoted)
                valueToString
                (Tuple.second n.input)

        renderElement x =
            [ ( indent, x ) ]

        renderChildren i =
            Tree.children tree
                |> List.concatMap (renderTree i)
    in
    case n.editionType of
        ET.OperatorT (ET.Operator name _ _) ->
            let
                prefix =
                    "(" ++ name ++ " "

                children =
                    renderChildren (indent + 1)
            in
            if r.isBottom then
                let
                    args =
                        String.join " " <| List.map Tuple.second children
                in
                renderElement <| prefix ++ args ++ ")"

            else
                let
                    xs =
                        ( indent, prefix ) :: children

                    lastIdx =
                        List.length xs - 1
                in
                List.indexedMap
                    (\i x ->
                        if i == lastIdx then
                            Tuple.mapSecond (\s -> s ++ ")") x

                        else
                            x
                    )
                    xs

        ET.OptArgT (ET.OptArg name _ _) ->
            case renderChildren indent of
                [] ->
                    []

                ( _, "nil" ) :: [] ->
                    []

                ( _, x ) :: tail ->
                    renderElement ("#:" ++ name ++ " " ++ x) ++ tail

        ET.ExpTypeT (S.ExpBaseType _) ->
            renderElement renderInput

        _ ->
            renderChildren indent


renderString : Tree ET.EditionNode -> String
renderString =
    let
        iStr =
            String.repeat 4 " "
    in
    buildRenderTree
        >> renderTree 0
        >> List.map (\( i, x ) -> String.repeat i iStr ++ String.trimRight x)
        >> String.join "\n"
