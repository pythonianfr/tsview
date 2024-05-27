module Editor.SpecParser exposing (..)

import Set
import Either exposing (Either(..))
import Maybe.Extra as Maybe

import Reader
import ReaderExtra exposing (ask, asks)
import AssocList as Assoc
import List.Nonempty as NE
import List.Nonempty.Ancillary as NEA
import Json.Decode as JD
import String.Format as SF
import Parser.Advanced as PA exposing ((|.), (|=))

import Editor.Type as T exposing (SpecErrors, Spec)


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


boolParser : Parser c x Bool
boolParser = asks .toToken <| \toToken -> PA.oneOf
    [ PA.succeed True |. (PA.keyword <| toToken "#t")
    , PA.succeed False |. (PA.keyword <| toToken "#f")
    ]

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

literalParser : T.LiteralType -> Parser c x T.LiteralExpr
literalParser literalType = case literalType of
    T.Bool -> boolParser |> map T.BoolExpr

    T.Int -> numParser "Int" PA.int |> map T.IntExpr

    T.Number -> numParser "Float" PA.float |> map T.NumberExpr

    T.String -> stringParser |> map T.StringExpr

    T.TimestampString -> stringParser |> map T.TimestampExpr

    T.SearchString -> stringParser |> map T.StringExpr


runParser :
   ProblemMap c x -> String -> Parser c x a -> Either T.ParserErrors a
runParser ({renderContext, renderProblem} as env) code pa =
    let
        convCtx {row, col, context} =
            T.Annotation row col (renderContext context)

        convDeadEnd : (PA.DeadEnd c x) -> T.ParserError
        convDeadEnd {row, col, problem, contextStack} =
            { annotation = T.Annotation row col (renderProblem problem)
            , contextStack = List.map convCtx contextStack |> NE.fromList
            }

        noErr =
            NE.singleton <| T.ParserError (T.Annotation 0 0 "NO ERR") Nothing

    in PA.run (Reader.run pa env) code
        |> Either.fromResult
        |> Either.mapLeft
            (List.map convDeadEnd >> NE.fromList >> Maybe.withDefault noErr)

renderParserErrors : T.ParserErrors -> String
renderParserErrors =
    let
        tab = String.repeat 4 " "

        renderLine : T.Annotation -> String
        renderLine {rowPos, colPos, errMess} =
            "({{ }}, {{ }}) =>  {{ }}"
                |> SF.value (String.fromInt rowPos)
                |> SF.value (String.fromInt colPos)
                |> SF.value errMess

        renderCtx : T.Annotation -> String
        renderCtx {rowPos, colPos, errMess} =
            "when parsing {{ }} at ({{ }}, {{ }})"
                |> SF.value errMess
                |> SF.value (String.fromInt rowPos)
                |> SF.value (String.fromInt colPos)
                |> String.append tab

        renderErr : T.ParserError -> List String
        renderErr {annotation, contextStack} = (renderLine annotation) ::
            (Maybe.unwrap [] (NE.map renderCtx >> NE.toList) contextStack)

    in
    (NE.toList >> List.concatMap renderErr >> String.join "\n")

parserErrorsToString : Either T.ParserErrors a -> Either String a
parserErrorsToString = Either.mapLeft renderParserErrors


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


-- Spec parser

literalTypeParser : Parser c x T.LiteralType
literalTypeParser = asks .toToken <| \toToken -> PA.oneOf
    [ PA.succeed T.Bool |. PA.keyword (toToken "bool") -- py
    , PA.succeed T.Bool |. PA.keyword (toToken "Bool")

    , PA.succeed T.Int |. PA.keyword (toToken "int") -- py
    , PA.succeed T.Int |. PA.keyword (toToken "Int")

    , PA.succeed T.Number |. PA.keyword (toToken "Number")

    , PA.succeed T.String |. PA.keyword (toToken "str") -- py
    , PA.succeed T.String |. PA.keyword (toToken "String")

    , PA.succeed T.TimestampString |. PA.keyword (toToken "TimestampString")
    , PA.succeed T.TimestampString |. PA.keyword (toToken "Timestamp")

    , PA.succeed T.SearchString |. PA.keyword (toToken "seriesname") -- py
    , PA.succeed T.SearchString |. PA.keyword (toToken "SearchString")
    ]

operatorOutputTypeParser : Parser c x T.OperatorOutputType
operatorOutputTypeParser = asks .expecting <| \expecting ->
    PA.variable
        { start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = expecting "OperatorOutputType"
        }
        |> PA.map T.OperatorOutputType

primitiveTypeParser : Parser c x T.PrimitiveType
primitiveTypeParser = ask <| \({toToken, internal} as env) ->
    let
        unionTypesParser : PA.Parser c x (List T.PrimitiveType)
        unionTypesParser =  PA.sequence
            { start = toToken "Union["
            , separator = toToken ","
            , end = toToken "]"
            , spaces = PA.spaces
            , item = PA.lazy (\_ -> Reader.run primitiveTypeParser env)
            , trailing = PA.Forbidden
            }

        toUnion : List T.PrimitiveType -> PA.Parser c x T.PrimitiveType
        toUnion xs = NE.fromList xs |> Maybe.unwrap
            (internal "Empty Union" |> PA.problem)
            (T.Union >> PA.succeed)

    in PA.oneOf
    [ unionTypesParser |> PA.andThen toUnion
    , Reader.run literalTypeParser env |> PA.map T.Literal
    , Reader.run operatorOutputTypeParser env |> PA.map T.OperatorOutput
    ]

parseContainer : T.Key -> String -> String -> Parser c x T.PrimitiveType
parseContainer key opening closing = ask <| \({toToken} as env) ->
    PA.succeed identity
        |. PA.keyword (toToken key)
        |. PA.symbol (toToken opening)
        |= Reader.run primitiveTypeParser env
        |. PA.keyword (toToken closing)

compositeTypeParser : Parser c x T.CompositeType
compositeTypeParser = ask <| \env -> PA.oneOf
    [ Reader.run (parseContainer "List" "[" "]") env |> PA.map T.VarArgs
    , Reader.run (parseContainer "Packed" "[" "]") env |> PA.map T.Packed
    ]

specTypeParser : Parser c x T.SpecType
specTypeParser = ask <| \env -> PA.oneOf
    [ Reader.run compositeTypeParser env |> PA.map T.CompositeType
    , Reader.run primitiveTypeParser env |> PA.map T.PrimitiveType
    ]

parseDefault : Parser c x ( T.SpecType, Maybe T.LiteralExpr )
parseDefault = ask <| \({toToken} as env) ->
    let
        parseNone = PA.succeed Nothing |. PA.keyword (toToken "None")

        parsePyBool = PA.map (Just << T.BoolExpr) <| PA.oneOf
            [ PA.succeed True |. PA.keyword (toToken "True")
            , PA.succeed False |. PA.keyword (toToken "False")
            ]

    in Reader.run specTypeParser env
        |. PA.symbol (toToken "=")
        |> PA.andThen (\t -> PA.map (Tuple.pair t) <| case t of
            T.PrimitiveType (T.Literal T.Bool) -> parsePyBool

            T.PrimitiveType (T.Literal literalType) -> PA.oneOf
                [ parseNone
                , Reader.run (literalParser literalType) env |> PA.map Just
                ]

            _ -> parseNone)

run : String -> Parser Never Problem a -> Either String a
run s pa =
    runParser (defaultProblemMap <| always "c") s pa |> parserErrorsToString


type alias EncodedArg =
    { key : T.Key
    , encodedValue : String
    }

type alias OperatorSpec =
    { operatorName : String
    , encodedArgs : NE.Nonempty EncodedArg
    }

type alias Errs a =
    (List String, a)

parseArgument : EncodedArg -> Errs T.Operator -> Errs T.Operator
parseArgument {key, encodedValue} (errs, op) =
    let
        step : Either String T.Operator -> Errs T.Operator
        step = Either.unpack (\e -> (e :: errs, op)) (Tuple.pair errs)
    in
    step <| run encodedValue <| ask <| \({toToken} as env) -> PA.oneOf
        [ PA.succeed (\x -> { op | optArgs = Assoc.insert key x op.optArgs})
            |. PA.keyword (toToken "Default")
            |. PA.symbol (toToken "[")
            |= Reader.run parseDefault env
            |. PA.symbol (toToken "]")
        , PA.succeed (\x -> { op | args = Assoc.insert key x op.args})
            |= Reader.run specTypeParser env
        ]

parseOperator : OperatorSpec -> Either (List String) T.Operator
parseOperator {operatorName, encodedArgs} =
    let
        {key, encodedValue} = NE.head encodedArgs
        encodedArgs_ = NE.tail encodedArgs

        toEither : Errs T.Operator -> Either (List String) T.Operator
        toEither (errs, op) =
            if (List.isEmpty errs) then (Right op) else (Left errs)

    in (run encodedValue <| ask <| \({internal} as env) ->
        if key == "return" then
            Reader.run specTypeParser env
        else
            PA.problem (internal "No return keyword as first argument"))
    |> Either.mapBoth
        List.singleton
        (T.Operator operatorName Assoc.empty Assoc.empty)
    |> Either.andThen
        (\op -> List.foldl parseArgument ([], op) encodedArgs_ |> toEither)

parseSpec : Result JD.Error (List OperatorSpec) -> (SpecErrors, Spec)
parseSpec jsonResult =
    let
        makeSpec : List T.Operator -> Spec
        makeSpec ops = Assoc.fromList <| List.map (\op -> (op.name, op)) ops

        partition :
            List (Either (List String) T.Operator) -> (SpecErrors, Spec)
        partition = Either.partition
            >> Tuple.mapBoth (List.concat >> NE.fromList) makeSpec

    in Either.fromResult jsonResult
        |> Either.mapLeft (JD.errorToString >> NE.singleton >> Just)
        |> Either.unpack
            (\errs -> (errs, Assoc.empty))
            (\ops -> List.map parseOperator ops |> partition)

operatorSpecDecoder : JD.Decoder OperatorSpec
operatorSpecDecoder =
    let d = JD.map2 EncodedArg (JD.index 0 JD.string) (JD.index 1 JD.string)
    in JD.map2
        OperatorSpec
        (JD.index 0 JD.string)
        (JD.index 1 <| NEA.decodeArray d)

parseSpecValue : JD.Value -> (SpecErrors, Spec)
parseSpecValue =
    JD.decodeValue (JD.list operatorSpecDecoder) >> parseSpec

parseSpecString : String -> (SpecErrors, Spec)
parseSpecString =
    JD.decodeString (JD.list operatorSpecDecoder) >> parseSpec
