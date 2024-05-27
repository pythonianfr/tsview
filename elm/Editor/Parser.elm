module Editor.Parser exposing (..)

import Set
import Dict
import Bool.Extra as Bool
import Tuple.Extra as Tuple
import Either exposing (Either(..))

import String.Format
import AssocList as Assoc
import List.NonEmpty as NE
import List.Extra exposing (allDifferentBy)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Parser.Extras as Parser

import Lisp
import Editor.Type as T exposing (TypedExpr, Spec, SpecType)


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


boolParser : Parser Bool
boolParser =
    Parser.oneOf
        [ Parser.succeed True |. Parser.keyword "#t"
        , Parser.succeed False |. Parser.keyword "#f"
        ]


boolToString : Bool -> String
boolToString x =
    case x of
        True ->
            "True"

        False ->
            "False"


valueParser : T.LiteralType -> Parser ( String, T.EditableValue )
valueParser inputType =
    let
        andThen f =
            Parser.andThen (f >> Parser.succeed)
    in
    case inputType of
        T.Bool ->
            boolParser
                |> andThen (\x -> ( boolToString x, T.BoolValue x ))

        T.Int ->
            numberParser Parser.int
                |> andThen (\x -> ( String.fromInt x, T.IntValue x ))

        T.Number ->
            numberParser Parser.float
                |> andThen (\x -> ( String.fromFloat x, T.NumberValue x ))

        T.String ->
            stringParser
                |> andThen (\x -> ( x, T.StringValue x ))

        T.TimestampString ->
            -- XXX should be a date parser ?
            stringParser
                |> andThen (\x -> ( x, T.TimestampValue x ))

        T.SearchString ->
            stringParser
                |> andThen (\x -> ( x, T.StringValue x ))


parseTypedExpr : Spec -> SpecType -> Parser TypedExpr
parseTypedExpr spec specType =
    case specType of
        T.Editable x -> Parser.oneOf
            [ Parser.map
                (Tuple.second >> T.TLiteral x)
                (valueParser x |. Parser.spaces)
            , parseOperator spec
            ]

        T.Series ->
            parseOperator spec

        T.Query ->
            parseOperator spec

        T.Timestamp ->
            parseOperator spec

        T.VarArgs x ->
            Parser.map (T.TVarArgs x) (parseList spec x)

        T.Packed x ->
            parseOperator spec

        T.Union xs ->
            Parser.map (T.TUnion xs) (parseUnion spec xs)


parseList : Spec -> SpecType -> Parser (List TypedExpr)
parseList spec specType =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ parseTypedExpr spec specType
                    |> Parser.map (\x -> x :: xs |> Parser.Loop)
                , Parser.succeed (List.reverse xs |> Parser.Done)
                ]
        )


parseUnion : Spec -> NE.NonEmpty SpecType -> Parser ( SpecType, TypedExpr )
parseUnion spec =
    NE.toList
        >> List.map (\x -> Parser.map (Tuple.pair x) (parseTypedExpr spec x))
        >> Parser.oneOf


type alias ArgsSpec =
    List ( T.Key, SpecType )


type alias OptArgsSpec =
    T.KAssoc SpecType


type alias Args =
    List ( T.Key, TypedExpr )


parseArgs : Spec -> ArgsSpec -> OptArgsSpec -> Args -> Parser Args
parseArgs spec argsSpec optArgsSpec args =
    case argsSpec of
        ( k, t ) :: xs ->
            let
                parsePositional : Parser Args
                parsePositional =
                    Parser.succeed (\v -> ( k, v ) :: args)
                        |. Parser.spaces
                        |= parseTypedExpr spec t
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
                |= parseTypedExpr spec t
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


parseOperatorArgs : Spec -> T.Operator -> Parser T.TypedExpr
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
        |> Parser.map (\( args, optArgs ) -> T.TOperator op args optArgs)


parseOperator : Spec -> Parser T.TypedExpr
parseOperator spec =
    let
        errMess = "could not find the operator {{ }} in the spec"

        getopspec opname =
            case Assoc.get opname spec of
                Just opspec ->
                    parseOperatorArgs spec opspec
                Nothing ->
                    Parser.problem <| String.Format.value opname <| errMess
    in
    Parser.between
        (Parser.symbol "(" |. Parser.spaces)
        (Parser.spaces |. Parser.symbol ")" |. Parser.spaces)
        (Parser.oneOf
            [ Lisp.symbolparser |. Parser.spaces |> Parser.andThen getopspec
            , Parser.succeed T.voidTOperator |. Parser.spaces
            ]
        )


parseFormula : Spec -> String -> Either String T.TypedExpr
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
