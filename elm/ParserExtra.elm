module ParserExtra exposing (..)

import Set
import Maybe.Extra as Maybe
import Either exposing (Either)

import Reader
import ReaderExtra exposing (ask, asks)
import List.Nonempty as NE
import String.Format as SF

import Parser.Advanced as PA exposing ((|.), (|=))


-- Parser.Advanced helpers

many : PA.Parser c x a -> PA.Parser c x (List a)
many p =
    let
        parseStep : (List a) -> PA.Parser c x (PA.Step (List a) (List a))
        parseStep xs = PA.oneOf
            [ PA.succeed (\x -> PA.Loop (x :: xs))
                |= p
                |. PA.spaces
            , PA.succeed (PA.Done (List.reverse xs))
            ]

    in PA.loop [] parseStep


some : PA.Parser c x a -> PA.Parser c x (a, List a)
some p = PA.succeed Tuple.pair
    |= p
    |. PA.spaces
    |= many p


between :
    PA.Parser c x opening ->
    PA.Parser c x closing ->
    PA.Parser c x a ->
    PA.Parser c x a
between opening closing p = PA.succeed identity
    |. opening
    |. PA.spaces
    |= p
    |. PA.spaces
    |. closing


empty : PA.Parser c x ()
empty = PA.succeed ()


-- Internal Parser library (stack Reader on top of PA.Parser)

type alias ProblemMap c x =
    { expecting : (String -> x)
    , toToken : (String -> PA.Token x)
    , invalid : (String -> x)
    , internal : (String -> x)
    , renderProblem : (x -> String)
    , renderContext : (c -> String)
    }


-- x is the Parser Problem (see Parser.Advanced)
type alias Parser c x a =
    Reader.Reader (ProblemMap c x) (PA.Parser c x a)


map : (a -> b) -> Parser c x a -> Parser c x b
map f = Reader.map (PA.map f)

map2 : (a -> b -> c) -> Parser c_ x a -> Parser c_ x b -> Parser c_ x c
map2 f = Reader.map2 (\a b -> PA.succeed f |= a |= b)

andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen f ma =
    let andThen_ : ProblemMap c x -> PA.Parser c x b
        andThen_ env =
            Reader.run ma env |> PA.andThen (\a -> Reader.run (f a) env)
    in Reader.asks andThen_

oneOf : List (Parser c x a) -> Parser c x a
oneOf xs = ask <| \env ->
    PA.oneOf <| List.map (\x -> Reader.run x env) xs


minusSign : Parser c x Bool
minusSign = asks .toToken <| \toToken -> PA.oneOf
    [ PA.map (always True) (PA.symbol <| toToken "-")
    , PA.succeed False
    ]

numParser : String -> (x -> x -> PA.Parser c x number) -> Parser c x number
numParser s pa = ask <| \({invalid, expecting} as env) ->
    PA.succeed (\negative v -> if negative then -v else v)
        |= Reader.run minusSign env
        |= pa (expecting s) (invalid s)

stringParser : Parser c x String
stringParser = ask <| \{expecting, toToken} ->
    let quote = Char.fromCode 34 -- "char"
        quoteSymbol = String.fromChar quote
        tokenSymbol = toToken quoteSymbol
    in PA.succeed identity
        |. PA.symbol tokenSymbol
        |= PA.variable
            { start = always True
            , inner = (/=) quote
            , reserved = Set.empty
            , expecting = expecting "String"
            }
        |. PA.symbol tokenSymbol


type alias Annotation =
    { rowPos : Int
    , colPos : Int
    , errMess : String
    }


type alias ParserError =
    { annotation : Annotation
    , contextStack : Maybe (NE.Nonempty Annotation)
    }


type alias ParserErrors = NE.Nonempty ParserError


runParser :
   ProblemMap c x -> String -> Parser c x a -> Either ParserErrors a
runParser ({renderContext, renderProblem} as env) code pa =
    let
        convCtx {row, col, context} =
            Annotation row col (renderContext context)

        convDeadEnd : (PA.DeadEnd c x) -> ParserError
        convDeadEnd {row, col, problem, contextStack} =
            { annotation = Annotation row col (renderProblem problem)
            , contextStack = List.map convCtx contextStack |> NE.fromList
            }

        noErr =
            NE.singleton <| ParserError (Annotation 0 0 "NO ERR") Nothing

    in PA.run (Reader.run pa env) code
        |> Either.fromResult
        |> Either.mapLeft
            (List.map convDeadEnd >> NE.fromList >> Maybe.withDefault noErr)

renderParserErrors : ParserErrors -> String
renderParserErrors =
    let
        tab = String.repeat 4 " "

        renderLine : Annotation -> String
        renderLine {rowPos, colPos, errMess} =
            "({{ }}, {{ }}) =>  {{ }}"
                |> SF.value (String.fromInt rowPos)
                |> SF.value (String.fromInt colPos)
                |> SF.value errMess

        renderCtx : Annotation -> String
        renderCtx {rowPos, colPos, errMess} =
            "when parsing {{ }} at ({{ }}, {{ }})"
                |> SF.value errMess
                |> SF.value (String.fromInt rowPos)
                |> SF.value (String.fromInt colPos)
                |> String.append tab

        renderErr : ParserError -> List String
        renderErr {annotation, contextStack} = (renderLine annotation) ::
            (Maybe.unwrap [] (NE.map renderCtx >> NE.toList) contextStack)

    in
    (NE.toList >> List.concatMap renderErr >> String.join "\n")

parserErrorsToString : Either ParserErrors a -> Either String a
parserErrorsToString = Either.mapLeft renderParserErrors

toParserError : String -> ParserError
toParserError s = ParserError (Annotation 0 0 s) Nothing


-- Parser with defaultProblemMap

type Problem
    = Expecting String
    | ExpectingToken String
    | Invalid String
    | Internal String


defaultProblemMap : (c -> String) -> ProblemMap c Problem
defaultProblemMap f =
    let
        renderProblem : Problem -> String
        renderProblem p = case p of
            Expecting s -> "Expecting: " ++  s
            ExpectingToken s -> "Expecting Token: " ++ s
            Invalid s -> "Invalid: " ++s
            Internal s -> "Internal error: " ++ s
    in
    { expecting = Expecting
    , toToken = (\s -> PA.Token s (ExpectingToken s))
    , invalid = Invalid
    , internal = Internal
    , renderProblem = renderProblem
    , renderContext = f
    }

run : String -> Parser Never Problem a -> Either String a
run s pa =
    runParser (defaultProblemMap <| always "c") s pa |> parserErrorsToString
