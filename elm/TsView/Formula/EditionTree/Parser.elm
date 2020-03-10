module TsView.Formula.EditionTree.Parser exposing (parseFormula)

import Dict
import Either exposing (Either(..))
import List.Extra exposing (allDifferentBy)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import Tree exposing (Tree)
import TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)
import TsView.Formula.EditionTree.Type as ET exposing (EditionNode)
import TsView.Formula.Spec.Type as S exposing (Spec)
import TsView.Formula.Utils
    exposing
        ( boolParser
        , boolToString
        , numberParser
        , stringParser
        )


logEditionTree : String -> EditionTree -> EditionTree
logEditionTree mess tree =
    -- Uncomment for logging
    --    let
    --        _ =
    --            Debug.log
    --                (String.join "\n" [ "", "=> " ++ mess, inspectEditionTree tree, "" ])
    --                "____"
    --    in
    tree


type alias EditionTree =
    Tree EditionNode


type alias Forest a =
    List (Tree a)


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    Parser.map f pa |= pb


traverse : (Tree a -> Parser (Tree b)) -> Forest a -> Parser (Forest b)
traverse parseTree forest =
    List.foldr
        (\a b -> map2 (::) (parseTree a) b)
        (Parser.succeed [])
        forest


{-| parse Tree storing current item and running Tree Parser on descendants
-}
treeParser : (Tree a -> Parser (Tree b)) -> b -> Tree a -> Parser (Tree b)
treeParser parseSubTree item tree =
    Parser.map (Tree.tree item) (Tree.children tree |> traverse parseSubTree)


type alias ToValue a =
    a -> ( String, ET.Value )


parseInputType : S.InputType -> S.Spec -> EditionTree -> Parser EditionTree
parseInputType inputType spec tree =
    let
        n =
            Tree.label tree

        inputParser : ToValue a -> Parser a -> Parser EditionTree
        inputParser toValue pa =
            pa
                |. Parser.spaces
                |> Parser.andThen
                    (\a ->
                        let
                            edited =
                                { n | input = toValue a |> Tuple.mapSecond Right }
                        in
                        treeParser (parseEditionNode spec) edited tree
                    )
    in
    case inputType of
        S.Bool ->
            boolParser
                |> inputParser (\x -> ( boolToString x, ET.BoolValue x ))

        S.Int ->
            numberParser Parser.int
                |> inputParser (\x -> ( String.fromInt x, ET.IntValue x ))

        S.Number ->
            numberParser Parser.float
                |> inputParser (\x -> ( String.fromFloat x, ET.NumberValue x ))

        S.String ->
            stringParser
                |> inputParser (\x -> ( x, ET.StringValue x ))

        S.Timestamp ->
            -- XXX should be a date parser ?
            stringParser
                |> inputParser (\x -> ( x, ET.TimestampValue x ))

        S.SearchString ->
            stringParser
                |> inputParser (\x -> ( x, ET.StringValue x ))


parseExpType : S.ExpType -> S.Spec -> EditionTree -> Parser EditionTree
parseExpType expType spec tree =
    let
        n =
            Tree.label tree
    in
    case expType of
        S.ExpBaseType (S.BaseInput x) ->
            parseInputType x spec tree

        S.ExpBaseType S.Series ->
            Parser.problem "Should have been handled by SelectorT Series"

        S.Union xs ->
            Parser.map (Tree.tree n) (parseUnionChoice spec xs)

        S.SList x ->
            Parser.map (Tree.tree n) (parseSList spec x)


parseEditionNode : S.Spec -> EditionTree -> Parser EditionTree
parseEditionNode spec tree =
    let
        n =
            Tree.label tree
    in
    case n.editionType of
        ET.ReturnTypeT _ ->
            -- XXX Should handle ReturnType and ReturnTypes
            Parser.map
                (List.singleton >> Tree.tree n)
                (ET.SelectorT S.Series
                    |> ET.buildEditionTree spec
                    |> parseEditionNode spec
                )
                |> Parser.map (logEditionTree "Formula Result")

        ET.SelectorT x ->
            Parser.map
                (List.singleton >> Tree.tree n)
                (parseOperator spec x)
                |> Parser.map (logEditionTree "SelectorT")

        ET.InputSelectorT x ->
            Parser.oneOf
                [ treeParser (parseEditionNode spec) n tree
                , Parser.map
                    (List.singleton >> Tree.tree n)
                    (parseOperator spec (S.BaseInput x) |> Parser.backtrackable)
                ]
                |> Parser.map (logEditionTree "InputSelectorT")

        ET.ArgTypeT (ET.ArgType x) ->
            parseExpType x spec tree
                |> Parser.map (logEditionTree "ArgTypeT")

        ET.OptArgsT _ ->
            Parser.map (Tree.tree n) (parseOptArgs spec <| Tree.children tree)

        _ ->
            treeParser (parseEditionNode spec) n tree


parseUnionChoice : Spec -> Nonempty S.ExpType -> Parser (Forest EditionNode)
parseUnionChoice spec =
    NE.toList
        >> List.map (ET.ArgType >> ET.probeArgSelector spec)
        >> List.map (ET.buildEditionTree spec >> parseEditionNode spec)
        >> Parser.oneOf
        >> Parser.map (logEditionTree "UnionChosen")
        >> Parser.map List.singleton


parseSList : Spec -> S.ExpType -> Parser (Forest EditionNode)
parseSList spec expType =
    let
        prs : Parser EditionTree
        prs =
            expType
                |> ET.ArgType
                |> ET.probeArgSelector spec
                |> ET.buildEditionTree spec
                |> parseEditionNode spec
    in
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ prs |> Parser.map (\x -> x :: xs |> Parser.Loop)
                , Parser.succeed (List.reverse xs |> Parser.Done)
                ]
        )


type alias OptArgs =
    List ( String, EditionTree )


type alias OptArgStep =
    Parser (Parser.Step OptArgs (Forest EditionNode))


parseOptArg : Spec -> OptArgs -> ( String, EditionTree ) -> OptArgStep
parseOptArg spec optArgs ( k, tree ) =
    Parser.succeed (\v -> ( k, v ) :: optArgs |> Parser.Loop)
        |. Parser.keyword ("#:" ++ k)
        |. Parser.spaces
        |= parseEditionNode spec tree


parseOptArgs : Spec -> Forest EditionNode -> Parser (Forest EditionNode)
parseOptArgs spec forest =
    let
        optArgs : OptArgs
        optArgs =
            List.foldr
                (\a b ->
                    case Tree.label a |> .editionType of
                        ET.OptArgT (ET.OptArg k _) ->
                            ( k, a ) :: b

                        _ ->
                            b
                )
                []
                forest

        optArgForest : OptArgs -> Forest EditionNode
        optArgForest parsedOptArgs =
            let
                d =
                    Dict.fromList parsedOptArgs

                finalOptArg ( k, v ) b =
                    (Dict.get k d |> Maybe.withDefault v) :: b
            in
            List.foldr finalOptArg [] optArgs
    in
    Parser.loop []
        (\xs ->
            let
                optArgParsers =
                    List.map (parseOptArg spec xs) optArgs

                end =
                    if allDifferentBy Tuple.first xs then
                        Parser.succeed <| Parser.Done <| optArgForest xs

                    else
                        Parser.problem "Duplicate keyword for optional argument"
            in
            Parser.oneOf <| List.append optArgParsers [ end ]
        )


parseOperator : S.Spec -> S.BaseType -> Parser EditionTree
parseOperator spec baseType =
    let
        parseKeyword : List S.Operator -> Parser ET.EditionType
        parseKeyword =
            List.map
                (\op ->
                    Parser.succeed (ET.fromSpecOperator op |> ET.OperatorT)
                        |. Parser.keyword op.name
                        |. Parser.spaces
                )
                >> Parser.oneOf

        parseOperatorKeyword : Parser ET.EditionType
        parseOperatorKeyword =
            S.getOperators baseType spec
                |> Maybe.map (NE.toList >> parseKeyword)
                |> Maybe.withDefault (Parser.problem "No operator")

        buildEditionTree : ET.EditionType -> Parser EditionTree
        buildEditionTree editionType =
            let
                opTree =
                    ET.buildEditionTree spec editionType
                        |> logEditionTree "ChosenOperator"
            in
            treeParser (parseEditionNode spec) (Tree.label opTree) opTree
    in
    (Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseOperatorKeyword
        |> Parser.andThen buildEditionTree
    )
        |. Parser.spaces
        |. Parser.symbol ")"
        |. Parser.spaces


parseFormula : Spec -> String -> Either String EditionTree
parseFormula spec formulaCode =
    let
        runParser =
            Parser.run
                (Parser.succeed identity
                    |. Parser.spaces
                    |= parseEditionNode spec (ET.buildInitialTree spec)
                    |. Parser.end
                )
    in
    formulaCode
        |> runParser
        |> Either.fromResult
        |> Either.mapLeft Parser.deadEndsToString
        |> Either.voidLeft "FAILED"
