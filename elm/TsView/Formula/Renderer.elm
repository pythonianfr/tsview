module TsView.Formula.Renderer exposing (renderString)

import Either exposing (Either(..))
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.Spec as S exposing (Model, Msg(..))


type alias RenderNode =
    { isBottom : Bool
    , node : S.EditionNode
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

        S.IntValue x ->
            String.fromInt x

        S.FloatValue x ->
            String.fromFloat x

        S.StringValue x ->
            quoted x

        S.DateValue x ->
            quoted x


buildRenderTree : Zipper S.EditionNode -> Tree RenderNode
buildRenderTree zipper =
    let
        n =
            Zipper.current zipper

        children =
            S.buildForest buildRenderTree zipper

        isNotOperator r =
            case r.node.specType of
                S.Operator _ _ _ ->
                    False

                _ ->
                    True

        isBottom =
            LL.toList children
                |> List.map Tree.item
                |> List.all (\r -> r.isBottom && isNotOperator r)
    in
    Tree.Tree (RenderNode isBottom n) children


renderTree : Int -> Zipper RenderNode -> Nonempty ( Int, String )
renderTree indent zipper =
    let
        r =
            Zipper.current zipper

        n =
            r.node

        renderInput : String
        renderInput =
            Either.unpack
                (\_ -> Tuple.first n.input |> quoted)
                valueToString
                (Tuple.second n.input)

        renderElement x =
            NE.fromElement ( indent, x )

        emptyElement =
            renderElement ""

        isEmpty ( _, s ) =
            s == ""

        renderChildren i =
            Zipper.openAll zipper
                |> NE.fromList
                |> Maybe.withDefault (NE.fromElement zipper)
                |> NE.concatMap (renderTree i)
    in
    case n.specType of
        S.Operator name _ _ ->
            let
                prefix =
                    "(" ++ name ++ " "

                children =
                    renderChildren (indent + 1)
                        |> NE.toList
                        |> List.filter (not << isEmpty)
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
                        NE.Nonempty ( indent, prefix ) children

                    lastIdx =
                        NE.length xs - 1
                in
                NE.indexedMap
                    (\i x ->
                        if i == lastIdx then
                            Tuple.mapSecond (\s -> s ++ ")") x

                        else
                            x
                    )
                    xs

        S.Arg _ ->
            renderChildren indent

        S.OptArg name _ ->
            let
                children =
                    renderChildren indent

                ( _, x ) =
                    NE.head <| children
            in
            if x == nil then
                emptyElement

            else
                NE.Nonempty
                    ( indent, "#:" ++ name ++ " " ++ x )
                    (NE.tail children)

        S.OptArgs x ->
            renderChildren indent

        S.Series ->
            renderChildren indent

        S.SList _ ->
            renderChildren indent

        S.Union _ ->
            renderChildren indent

        _ ->
            renderElement <| renderInput


renderString : Zipper S.EditionNode -> String
renderString =
    let
        iStr =
            String.repeat 4 " "
    in
    buildRenderTree
        >> Zipper.fromTree
        >> renderTree 0
        >> NE.toList
        >> List.map (\( i, x ) -> String.repeat i iStr ++ String.trimRight x)
        >> String.join "\n"
