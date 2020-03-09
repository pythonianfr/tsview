module TsView.Formula.EditionTree.Parser exposing (parseFormula)

import Dict
import Either exposing (Either(..))
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Forest, Tree(..))
import List.Extra exposing (allDifferentBy)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)
import TsView.Formula.EditionTree.Type as ET exposing (EditionNode)
import TsView.Formula.Spec.Type as S exposing (Spec)
import TsView.Formula.Utils exposing (numberParser, stringParser)


log : String -> EditionTree -> EditionTree
log mess tree =
    let
        _ =
            Debug.log
                (String.join "\n" [ "", "=> " ++ mess, inspectEditionTree tree, "" ])
                "____"
    in
    tree


type alias EditionTree =
    Tree EditionNode


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    Parser.map f pa |= pb


traverse : (Tree a -> Parser (Tree b)) -> Forest a -> Parser (Forest b)
traverse parseTree forest =
    LL.foldr
        (\a b -> map2 LL.cons (parseTree a) b)
        (Parser.succeed LL.empty)
        forest


{-| parse Tree storing current item and running Tree Parser on descendants
-}
treeParser : (Tree a -> Parser (Tree b)) -> b -> Tree a -> Parser (Tree b)
treeParser parseSubTree item tree =
    Parser.map (Tree.Tree item) (Tree.descendants tree |> traverse parseSubTree)


type alias ToValue a =
    a -> ( String, ET.Value )


parseInputType : S.InputType -> S.Spec -> EditionTree -> Parser EditionTree
parseInputType inputType spec tree =
    let
        n =
            Tree.item tree

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
            stringParser
                |> inputParser (\x -> ( x, ET.BoolValue True ))

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
            Tree.item tree
    in
    case expType of
        S.ExpBaseType (S.BaseInput x) ->
            parseInputType x spec tree

        S.ExpBaseType S.Series ->
            Parser.problem "Should have been handled by SelectorT Series"

        S.Union xs ->
            Parser.map (Tree.Tree n) (parseUnionChoice spec xs)

        S.SList x ->
            Parser.map (Tree.Tree n) (parseSList spec x)


parseEditionNode : S.Spec -> EditionTree -> Parser EditionTree
parseEditionNode spec tree =
    let
        n =
            Tree.item tree
    in
    case n.editionType of
        ET.ReturnTypeT _ ->
            -- XXX Should handle ReturnType and ReturnTypes
            Parser.map
                (LL.singleton >> Tree.Tree n)
                (ET.SelectorT S.Series
                    |> ET.buildEditionTree spec
                    |> parseEditionNode spec
                )
                |> Parser.map (log "Formula Result")

        ET.SelectorT x ->
            Parser.map
                (LL.singleton >> Tree.Tree n)
                (parseOperator spec x)
                |> Parser.map (log "SelectorT")

        ET.ArgTypeT (ET.ArgType x) ->
            parseExpType x spec tree
                |> Parser.map (log "ArgTypeT")

        ET.OptArgsT _ ->
            Parser.map (Tree.Tree n) (parseOptArgs spec <| Tree.descendants tree)

        ET.ArgT _ ->
            treeParser (parseEditionNode spec) n tree

        _ ->
            treeParser (parseEditionNode spec) n tree


parseUnionChoice : Spec -> Nonempty S.ExpType -> Parser (Forest EditionNode)
parseUnionChoice spec =
    NE.toList
        >> Debug.log "Union"
        >> List.map (ET.ArgType >> ET.probeArgSelector spec)
        >> List.map (ET.buildEditionTree spec >> parseEditionNode spec)
        >> Parser.oneOf
        >> Parser.map (log "UnionChosen")
        >> Parser.map LL.singleton


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
    Parser.loop LL.empty
        (\xs ->
            Parser.oneOf
                [ prs |> Parser.map (\x -> LL.cons x xs |> Parser.Loop)
                , Parser.succeed (LL.reverse xs |> Parser.Done)
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
            LL.foldr
                (\a b ->
                    case Tree.item a |> .editionType of
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
            List.foldr finalOptArg [] optArgs |> LL.fromList
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
                        |> log "ChosenOperator"
            in
            treeParser (parseEditionNode spec) (Tree.item opTree) opTree
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
    Debug.log "PARSING" formulaCode
        |> runParser
        |> Either.fromResult
        |> Either.mapLeft Parser.deadEndsToString
        |> Either.voidLeft "FAILED"
