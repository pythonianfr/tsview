module TsView.Formula.Parser exposing (parseSpec, stringParser)

import Dict
import Either exposing (Either(..))
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Forest, Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Extra exposing (allDifferentBy)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import Set
import TsView.Formula.Spec as S


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    Parser.map f pa |= pb


traverse : (Tree a -> Parser (Tree b)) -> Forest a -> Parser (Forest b)
traverse parseTree =
    LL.foldr
        (\a b -> map2 LL.cons (parseTree a) b)
        (Parser.succeed LL.empty)


{-| parse Tree storing current item and running Tree Parser on descendants
-}
treeParser : (Tree a -> Parser (Tree b)) -> b -> Tree a -> Parser (Tree b)
treeParser parseSubTree item tree =
    Parser.map (Tree.Tree item) (Tree.descendants tree |> traverse parseSubTree)


type alias ToValue a =
    a -> ( String, S.Value )


stringParser : Parser String
stringParser =
    let
        quote =
            '"'

        quoteSymbol =
            String.fromChar quote
    in
    Parser.succeed identity
        |. Parser.symbol quoteSymbol
        |= Parser.variable
            { start = always True
            , inner = (/=) quote
            , reserved = Set.empty
            }
        |. Parser.symbol quoteSymbol


minusSign : Parser Bool
minusSign =
    Parser.oneOf
        [ Parser.map (always True) (Parser.symbol "-")
        , Parser.succeed False
        ]


numberParser : Parser number -> Parser number
numberParser pa =
    Parser.succeed
        (\negative x ->
            if negative then
                -x

            else
                x
        )
        |= minusSign
        |= pa


parseEditionNode : S.Spec -> Tree S.EditionNode -> Parser (Tree S.EditionNode)
parseEditionNode spec tree =
    let
        n =
            Tree.item tree

        inputParser : ToValue a -> Parser a -> Parser (Tree S.EditionNode)
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
    case n.specType of
        S.Int ->
            numberParser Parser.int
                |> inputParser (\x -> ( String.fromInt x, S.IntValue x ))

        S.Float ->
            numberParser Parser.float
                |> inputParser (\x -> ( String.fromFloat x, S.FloatValue x ))

        S.String ->
            stringParser
                |> inputParser (\x -> ( x, S.StringValue x ))

        S.Date ->
            -- XXX should be a date parser ?
            stringParser
                |> inputParser (\x -> ( x, S.DateValue x ))

        S.SearchString ->
            stringParser
                |> inputParser (\x -> ( x, S.StringValue x ))

        S.Series ->
            parseOperator spec

        S.Union specTypes ->
            Parser.map (Tree.Tree n) (parseUnionChoice spec specTypes)

        S.SList specType ->
            Parser.map (Tree.Tree n) (parseSList spec specType)

        S.OptArgs _ ->
            Parser.map (Tree.Tree n) (parseOptArgs spec <| Tree.descendants tree)

        _ ->
            treeParser (parseEditionNode spec) n tree


parseUnionChoice : S.Spec -> Nonempty S.SpecType -> Parser (Forest S.EditionNode)
parseUnionChoice spec =
    NE.toList
        >> List.map (S.buildEditionTree spec >> parseEditionNode spec)
        >> Parser.oneOf
        >> Parser.map LL.singleton


parseSList : S.Spec -> S.SpecType -> Parser (Forest S.EditionNode)
parseSList spec specType =
    Parser.loop LL.empty
        (\xs ->
            Parser.oneOf
                [ S.buildEditionTree spec specType
                    |> parseEditionNode spec
                    |> Parser.map (\x -> LL.cons x xs |> Parser.Loop)
                , Parser.succeed (LL.reverse xs |> Parser.Done)
                ]
        )


type alias OptArgs =
    List ( String, Tree S.EditionNode )


type alias OptArgStep =
    Parser (Parser.Step OptArgs (Forest S.EditionNode))


parseOptArg : S.Spec -> OptArgs -> ( String, Tree S.EditionNode ) -> OptArgStep
parseOptArg spec optArgs ( k, tree ) =
    Parser.succeed (\v -> ( k, v ) :: optArgs |> Parser.Loop)
        |. Parser.keyword ("#:" ++ k)
        |. Parser.spaces
        |= parseEditionNode spec tree


parseOptArgs : S.Spec -> Forest S.EditionNode -> Parser (Forest S.EditionNode)
parseOptArgs spec forest =
    let
        optArgs : OptArgs
        optArgs =
            LL.foldr
                (\a b ->
                    case Tree.item a |> .specType of
                        S.OptArg k _ ->
                            ( k, a ) :: b

                        _ ->
                            b
                )
                []
                forest

        optArgForest : OptArgs -> Forest S.EditionNode
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


parseOperator : S.Spec -> Parser (Tree S.EditionNode)
parseOperator spec =
    let
        parseOperatorKeyword : Parser S.SpecType
        parseOperatorKeyword =
            S.listOperators spec
                |> List.map
                    (\( k, t ) ->
                        Parser.succeed t
                            |. Parser.keyword k
                            |. Parser.spaces
                    )
                |> Parser.oneOf

        buildEditionTree : S.SpecType -> Parser (Tree S.EditionNode)
        buildEditionTree specType =
            let
                opTree =
                    S.buildEditionTree spec specType
            in
            treeParser (parseEditionNode spec) (Tree.item opTree) opTree
    in
    (Parser.succeed identity
        |. Parser.spaces
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseOperatorKeyword
        |> Parser.andThen buildEditionTree
    )
        |. Parser.spaces
        |. Parser.symbol ")"
        |. Parser.spaces


parseSpec : S.Spec -> String -> Either String (Tree S.EditionNode)
parseSpec spec code =
    let
        res =
            Parser.run (parseOperator spec |. Parser.end) code
    in
    res
        |> Either.fromResult
        |> Either.mapLeft Parser.deadEndsToString
        |> Either.voidLeft "FAILED"
