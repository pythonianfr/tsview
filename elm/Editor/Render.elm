module Editor.Render exposing (renderFormula)

import Bool.Extra as Bool
import Either exposing (Either(..))

import AssocList as Assoc
import List.Extra as List
import Tree exposing (Tree)

import Editor.Type as T


type RenderType
    = Root
    | Operator String
    | Arg
    | OptArg String
    | Value T.EditableValue


type alias RenderNode =
    { isBottom : Bool
    , rtype : RenderType
    }


nil : String
nil = "nil"

quoted : String -> String
quoted x = "\"" ++ x ++ "\""

valueToString : T.EditableValue -> String
valueToString value = case value of
    T.Nil ->  nil

    T.BoolValue True -> "#t"

    T.BoolValue False -> "#f"

    T.IntValue x -> String.fromInt x

    T.NumberValue x -> String.fromFloat x

    T.StringValue x -> quoted x

    T.TimestampValue x -> quoted x

buildTypeTree : T.TypedExpr -> List (Tree RenderType)
buildTypeTree sexpr = case sexpr of
    T.TLiteral _ T.Nil -> []

    T.TLiteral _ x -> [ Tree.singleton (Value x) ]

    T.TVarArgs _ xs -> List.concatMap buildTypeTree xs

    T.TUnion _ ( _, x ) -> buildTypeTree x

    T.TOperator op xs ys ->
        let
            args = List.map
                (buildTypeTree >> Tree.tree Arg)
                (Assoc.values xs)

            optArgs = List.map
                (\( k, v ) -> Tree.tree (OptArg k) (buildTypeTree v))
                (Assoc.toList ys)

        in [ Tree.tree (Operator op.name) (args ++ optArgs) ]

buildRenderTree : Tree RenderType -> Tree RenderNode
buildRenderTree tree =
    let
        n = Tree.label tree

        forest = Tree.children tree |> List.map buildRenderTree

        isNotOperator r = case r.rtype of
            Operator _ -> False
            _ -> True

        isBottom = forest
            |> List.map Tree.label
            |> List.all (\r -> r.isBottom && isNotOperator r)
    in
    Tree.tree (RenderNode isBottom n) forest


renderTree : Int -> Tree RenderNode -> List ( Int, String )
renderTree indent tree =
    let
        r = Tree.label tree
        renderElement x = [ ( indent, x ) ]
        renderChildren i =
            Tree.children tree |> List.concatMap (renderTree i)

    in case r.rtype of
        Root -> renderChildren indent

        Operator opName ->
            let
                name = if (opName == "__void__") then "" else opName
                children = renderChildren (indent + 1)
                prefix =
                    "(" ++ name ++ Bool.ifElse "" " " (List.isEmpty children)
            in
            if r.isBottom then
                let args = String.join " " <| List.map Tuple.second children
                in renderElement <| prefix ++ args ++ ")"
            else
                let xs = (indent, prefix) :: children
                    lastIdx = List.length xs - 1
                in List.indexedMap (\i x ->
                    if i == lastIdx then Tuple.mapSecond (\s -> s ++ ")") x
                    else x) xs

        Arg -> renderChildren indent

        OptArg key -> List.updateAt
            0
            (\(_, x) -> ( indent, "#:" ++ key ++ " " ++ x ))
            (renderChildren indent)

        Value x -> renderElement (valueToString x)


renderFormula : T.TypedExpr -> String
renderFormula =
    let
        iStr = String.repeat 4 " "
    in buildTypeTree
        >> Tree.tree Root
        >> buildRenderTree
        >> renderTree 0
        >> List.map (\( i, x ) -> String.repeat i iStr ++ String.trimRight x)
        >> String.join "\n"


inspectTypedExpr : T.TypedExpr -> String
inspectTypedExpr = Debug.todo "inspecTypedExpr"
