module TshLisp.Parser exposing (..)

import Either exposing (Either(..))
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Parser.Extras as Parser
import Set
import TshLisp.Type as T exposing (LiteralValue, SExpr)


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


valueParser : Parser LiteralValue
valueParser =
    Parser.oneOf
        [ Parser.succeed T.NIL |. Parser.keyword "nil"
        , Parser.map T.BoolValue boolParser
        , Parser.map T.NumberValue (numberParser Parser.float)
        , Parser.map T.StringValue stringParser
        ]


sexprParser : Parser SExpr
sexprParser =
    Parser.oneOf
        [ Parser.map T.SInput valueParser
        , Parser.lazy (\_ -> operatorParser)
        ]


keywordParser : Parser String
keywordParser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


operatorSymbolParsers : List (Parser String)
operatorSymbolParsers =
    List.map
        (\x -> Parser.succeed x |. Parser.symbol x)
        [ "+", "*", "/" ]


operatorParser : Parser SExpr
operatorParser =
    let
        symbolParser : Parser String
        symbolParser =
            Parser.oneOf (keywordParser :: operatorSymbolParsers)

        keyArgParser : Parser ( String, SExpr )
        keyArgParser =
            Parser.succeed Tuple.pair
                |. Parser.symbol "#:"
                |= keywordParser
                |. Parser.spaces
                |= sexprParser
    in
    Parser.parens <|
        Parser.succeed T.SOperator
            |= symbolParser
            |. Parser.spaces
            |= Parser.many sexprParser
            |. Parser.spaces
            |= Parser.many keyArgParser


fullExprParser =
    Parser.succeed identity
        |. Parser.spaces
        |= operatorParser
        |. Parser.spaces
        |. Parser.end


parse : String -> Either String SExpr
parse =
    Parser.run fullExprParser
        >> Either.fromResult
        >> Either.mapLeft deadEndsToString


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
