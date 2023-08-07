module TsView.Formula.Parser exposing (..)

import AssocList as Assoc
import Dict
import Either exposing (Either(..))
import List.Extra exposing (allDifferentBy)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import TsView.Formula.Type as T exposing (SExpr, Spec, SpecType)
import TsView.Formula.Utils2 exposing (valueParser)
import Tuple.Extra as Tuple


parseSExpr : Spec -> SpecType -> Parser SExpr
parseSExpr spec specType =
    case specType of
        T.BaseInput x ->
            Parser.oneOf
                [ Parser.map (T.SInput x) (valueParser x |. Parser.spaces)
                , parseOperator spec
                ]

        T.Series ->
            Parser.map T.SSeries (parseOperator spec)

        T.List x ->
            Parser.map (T.SList x) (parseList spec x)

        T.Union xs ->
            Parser.map (T.SUnion xs) (parseUnion spec xs)


parseList : Spec -> SpecType -> Parser (List SExpr)
parseList spec specType =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ parseSExpr spec specType
                    |> Parser.map (\x -> x :: xs |> Parser.Loop)
                , Parser.succeed (List.reverse xs |> Parser.Done)
                ]
        )


parseUnion : Spec -> Nonempty SpecType -> Parser ( SpecType, SExpr )
parseUnion spec =
    NE.toList
        >> List.map (\x -> Parser.map (Tuple.pair x) (parseSExpr spec x))
        >> Parser.oneOf


type alias ArgsSpec =
    List ( T.Key, SpecType )


type alias OptArgsSpec =
    T.KAssoc SpecType


type alias Args =
    List ( T.Key, SExpr )


parseArgs : Spec -> ArgsSpec -> OptArgsSpec -> Args -> Parser Args
parseArgs spec argsSpec optArgsSpec args =
    case argsSpec of
        ( k, t ) :: xs ->
            let
                parsePositional : Parser Args
                parsePositional =
                    Parser.succeed (\v -> ( k, v ) :: args)
                        |. Parser.spaces
                        |= parseSExpr spec t
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
                |= parseSExpr spec t
    in
    Parser.loop args
        (\xs ->
            let
                optArgParsers =
                    List.map
                        (parseOptArg xs)
                        (argsSpec ++ Assoc.toList optArgsSpec)

                end =
                    if allDifferentBy Tuple.first xs then
                        Parser.succeed <| Parser.Done xs

                    else
                        Parser.problem "Duplicate keyword for argument"
            in
            Parser.oneOf (optArgParsers ++ [ end ])
        )


parseOperatorArgs : Spec -> T.Operator -> Parser T.SExpr
parseOperatorArgs spec op =
    let
        argKeys =
            Assoc.keys op.args

        optArgKeys =
            Assoc.keys op.optArgs

        keyIdx =
            List.indexedMap Tuple.pair (argKeys ++ optArgKeys)
                |> List.map Tuple.flip
                |> Dict.fromList

        sortByKeys =
            List.sortBy
                (\( k, _ ) ->
                    Dict.get k keyIdx
                        |> Maybe.withDefault -1
                )
                >> Assoc.fromList

        checkArgs ( args, optArgs ) =
            if Assoc.size args == List.length argKeys then
                Parser.succeed ( args, optArgs )

            else
                Parser.problem "Lack of mandatory argument"
    in
    parseArgs
        spec
        (Assoc.toList op.args)
        (Assoc.map (\_ x -> Tuple.first x) op.optArgs)
        []
        |> Parser.map sortByKeys
        |> Parser.map (Assoc.partition (\k _ -> List.member k argKeys))
        |> Parser.andThen checkArgs
        |> Parser.map (\( args, optArgs ) -> T.SOperator op args optArgs)


parseOperator : Spec -> Parser T.SExpr
parseOperator spec =
    let
        parseOperatorKeyword =
            List.map
                (\op ->
                    Parser.succeed op
                        |. Parser.keyword op.name
                        |. Parser.spaces
                        |> Parser.andThen (parseOperatorArgs spec)
                )
                (Assoc.values spec)
                |> Parser.oneOf
    in
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseOperatorKeyword
        |. Parser.spaces
        |. Parser.symbol ")"
        |. Parser.spaces


parseFormula : Spec -> String -> Either String T.SExpr
parseFormula spec formulaCode =
    let
        runParser =
            Parser.run
                (Parser.succeed identity
                    |. Parser.spaces
                    |= parseOperator spec
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
                        ++ " "
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
