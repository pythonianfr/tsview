module TshLisp.Render exposing (render)

import Bool.Extra as Bool
import List.Extra as List
import Tree exposing (Tree)
import TshLisp.Type as T exposing (LiteralValue, SExpr)


type RenderType
    = Root
    | Operator String
    | Arg
    | OptArg String
    | Value LiteralValue


type alias RenderNode =
    { isBottom : Bool
    , rtype : RenderType
    }


nil : String
nil =
    "nil"


quoted : String -> String
quoted x =
    "\"" ++ x ++ "\""


valueToString : LiteralValue -> String
valueToString value =
    case value of
        T.NIL ->
            nil

        T.BoolValue True ->
            "#t"

        T.BoolValue False ->
            "#f"

        T.NumberValue x ->
            String.fromFloat x

        T.StringValue x ->
            quoted x


buildTypeTree : SExpr -> List (Tree RenderType)
buildTypeTree sexpr =
    case sexpr of
        T.SInput x ->
            [ Tree.singleton (Value x) ]

        T.SOperator name xs ys ->
            let
                args =
                    List.map
                        (buildTypeTree >> Tree.tree Arg)
                        xs

                optArgs =
                    List.map
                        (\( k, v ) -> Tree.tree (OptArg k) (buildTypeTree v))
                        ys
            in
            [ Tree.tree (Operator name) (args ++ optArgs) ]


buildRenderTree : Tree RenderType -> Tree RenderNode
buildRenderTree tree =
    let
        n =
            Tree.label tree

        forest =
            Tree.children tree
                |> List.map buildRenderTree

        isNotOperator r =
            case r.rtype of
                Operator _ ->
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

        renderElement x =
            [ ( indent, x ) ]

        renderChildren i =
            Tree.children tree
                |> List.concatMap (renderTree i)
    in
    case r.rtype of
        Root ->
            renderChildren indent

        Operator name ->
            let
                children =
                    renderChildren (indent + 1)

                prefix =
                    "(" ++ name ++ Bool.ifElse "" " " (List.isEmpty children)
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

        Arg ->
            renderChildren indent

        OptArg key ->
            List.updateAt
                0
                (\( _, x ) -> ( indent, "#:" ++ key ++ " " ++ x ))
                (renderChildren indent)

        Value x ->
            renderElement (valueToString x)


render : SExpr -> String
render =
    let
        iStr =
            String.repeat 4 " "
    in
    buildTypeTree
        >> Tree.tree Root
        >> buildRenderTree
        >> renderTree 0
        >> List.map (\( i, x ) -> String.repeat i iStr ++ String.trimRight x)
        >> String.join "\n"
