module TsView.Formula.EditionTree.Parser exposing (parseFormula)

import AssocList as Assoc
import Dict
import Either exposing (Either(..))
import List.Extra as List
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Tree exposing (Tree)
import TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)
import TsView.Formula.EditionTree.Type as ET exposing (EditionNode, Forest)
import TsView.Formula.Spec.Type as S exposing (Spec)
import TsView.Formula.Utils exposing (valueParser)
import Tuple.Extra as Tuple


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


parseInputType : S.InputType -> S.Spec -> EditionTree -> Parser EditionTree
parseInputType inputType spec tree =
    let
        n =
            Tree.label tree

        toInput : ( String, S.Value ) -> ET.Input
        toInput =
            Tuple.mapSecond Right
    in
    valueParser inputType
        |. Parser.spaces
        |> Parser.andThen
            (\x ->
                treeParser (parseEditionNode spec) { n | input = toInput x } tree
            )


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

        ET.ExpTypeT x ->
            parseExpType x spec tree
                |> Parser.map (logEditionTree "ArgTypeT")

        _ ->
            treeParser (parseEditionNode spec) n tree


parseUnionChoice : Spec -> Nonempty S.ExpType -> Parser (Forest EditionNode)
parseUnionChoice spec =
    NE.toList
        >> List.map (ET.probeArgSelector spec)
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


type alias ArgsSpec =
    List ( String, EditionTree )


type alias OptArgsSpec =
    Assoc.Dict String EditionTree


type alias Args =
    List ( String, EditionTree )


parseArgs : Spec -> ArgsSpec -> OptArgsSpec -> Args -> Parser Args
parseArgs spec argsSpec optArgsSpec args =
    case argsSpec of
        ( k, t ) :: xs ->
            let
                parsePositional : Parser Args
                parsePositional =
                    Parser.succeed (\v -> ( k, v ) :: args)
                        |. Parser.spaces
                        |= parseEditionNode spec t
            in
            Parser.oneOf
                [ parsePositional |> Parser.andThen (parseArgs spec xs optArgsSpec)
                , parseArgsWithKey spec argsSpec optArgsSpec args
                ]

        [] ->
            parseArgsWithKey spec [] optArgsSpec args


parseArgsWithKey : Spec -> ArgsSpec -> OptArgsSpec -> Args -> Parser Args
parseArgsWithKey spec argsSpec optArgsSpec args =
    let
        parseOptArg ks ( k, t ) =
            Parser.succeed (\v -> ( k, v ) :: ks |> Parser.Loop)
                |. Parser.keyword ("#:" ++ k)
                |. Parser.spaces
                |= parseEditionNode spec t
    in
    Parser.loop args
        (\xs ->
            let
                optArgParsers =
                    List.map
                        (parseOptArg xs)
                        (argsSpec ++ Assoc.toList optArgsSpec)

                end =
                    if List.allDifferentBy Tuple.first xs then
                        Parser.succeed <| Parser.Done xs

                    else
                        Parser.problem "Duplicate keyword for argument"
            in
            Parser.oneOf (optArgParsers ++ [ end ])
        )


parseOperatorArgs : Spec -> ET.Operator -> Parser EditionTree
parseOperatorArgs spec ((ET.Operator _ args optArgs) as op) =
    let
        argsSpec =
            List.map
                (\((ET.Arg k t) as arg) ->
                    ET.buildEditionTree spec (ET.ArgT arg)
                        |> Tuple.pair k
                )
                args

        optArgsSpec =
            case optArgs of
                ET.OptArgs xs ->
                    List.map
                        (\((ET.OptArg k t _) as optArg) ->
                            ET.buildEditionTree spec (ET.OptArgT optArg)
                                |> Tuple.pair k
                        )
                        (NE.toList xs)
                        |> Assoc.fromList

                ET.NoOptArgs ->
                    Assoc.empty

        argsKeys =
            List.map Tuple.first argsSpec

        optArgsKeys =
            Assoc.keys optArgsSpec

        keyIdx =
            List.indexedMap Tuple.pair (argsKeys ++ optArgsKeys)
                |> List.map Tuple.flip
                |> Dict.fromList

        sortByKeys =
            Assoc.toList
                >> List.sortBy
                    (\( k, _ ) ->
                        Dict.get k keyIdx
                            |> Maybe.withDefault -1
                    )
                >> Assoc.fromList

        checkArgs ( kArgs, kOptArgs ) =
            if Assoc.size kArgs == List.length argsKeys then
                Parser.succeed ( kArgs, kOptArgs )

            else
                Parser.problem "Lack of mandatory argument"

        makeOpTree ( kArgs, kOptArgs ) =
            let
                optArgsNode =
                    case optArgs of
                        ET.NoOptArgs ->
                            []

                        x ->
                            Tree.tree
                                (ET.fromEditionType <| ET.OptArgsT x)
                                (Assoc.values kOptArgs)
                                |> List.singleton
            in
            Tree.tree
                (ET.fromEditionType <| ET.OperatorT op)
                (Assoc.values kArgs ++ optArgsNode)
    in
    parseArgs
        spec
        argsSpec
        optArgsSpec
        []
        |> Parser.map Assoc.fromList
        |> Parser.map (\ks -> Assoc.union ks optArgsSpec)
        |> Parser.map sortByKeys
        |> Parser.map (Assoc.partition (\k _ -> List.member k argsKeys))
        |> Parser.andThen checkArgs
        |> Parser.map makeOpTree


parseOperator : S.Spec -> S.BaseType -> Parser EditionTree
parseOperator spec baseType =
    let
        parseKeyword : List S.Operator -> Parser EditionTree
        parseKeyword =
            List.map
                (\op ->
                    Parser.succeed (ET.fromSpecOperator op)
                        |. Parser.keyword op.name
                        |. Parser.spaces
                )
                >> Parser.oneOf
                >> Parser.andThen (parseOperatorArgs spec)

        parseOperatorKeyword : Parser EditionTree
        parseOperatorKeyword =
            S.getOperators baseType spec
                |> Maybe.map (NE.toList >> parseKeyword)
                |> Maybe.withDefault (Parser.problem "No operator")
    in
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseOperatorKeyword
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
        |> Either.mapLeft deadEndsToString


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:"
                        ++ String.fromInt deadEnd.row
                        ++ " col:"
                        ++ String.fromInt deadEnd.col
                        ++ "\n"
            in
            case deadEnd.problem of
                Expecting str ->
                    "Expecting " ++ str ++ "at " ++ position

                ExpectingInt ->
                    "ExpectingInt at " ++ position

                ExpectingHex ->
                    "ExpectingHex at " ++ position

                ExpectingOctal ->
                    "ExpectingOctal at " ++ position

                ExpectingBinary ->
                    "ExpectingBinary at " ++ position

                ExpectingFloat ->
                    "ExpectingFloat at " ++ position

                ExpectingNumber ->
                    "ExpectingNumber at " ++ position

                ExpectingVariable ->
                    "ExpectingVariable at " ++ position

                ExpectingSymbol str ->
                    "ExpectingSymbol " ++ str ++ " at " ++ position

                ExpectingKeyword str ->
                    "ExpectingKeyword " ++ str ++ "at " ++ position

                ExpectingEnd ->
                    "ExpectingEnd at " ++ position

                UnexpectedChar ->
                    "UnexpectedChar at " ++ position

                Problem str ->
                    "ProblemString " ++ str ++ " at " ++ position

                BadRepeat ->
                    "BadRepeat at " ++ position
    in
    List.foldl (++) "" (List.map deadEndToString deadEnds)
