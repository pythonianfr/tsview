module Editor.SpecParser exposing (..)

import Set
import Array
import Maybe.Extra as Maybe
import Either exposing (Either(..))

import Reader
import List.Nonempty as NE
import List.Nonempty.Ancillary as NEA
import Json.Decode as JD
import Parser.Advanced as PA exposing ((|.), (|=))
import Optics.Core as O exposing (o)

import AssocList as Assoc
import ReaderExtra exposing (ask, asks)
import ParserExtra as PE exposing (Parser)
import OpticsExtra as OE

import Editor.Type as T exposing (SpecErrors, Spec)


type alias EncodedArg =
    { key : T.Key
    , encodedValue : String
    }

type alias OperatorSpec =
    { operatorName : String
    , encodedArgs : NE.Nonempty EncodedArg
    }

type alias Arg =
    { isOpt : Bool
    , key : T.Key
    , argType : T.ArgType
    , defaultExpr : Maybe T.LiteralExpr
    }


boolParser : Parser c x Bool
boolParser = asks .toToken <| \toToken -> PA.oneOf
    [ PA.succeed True |. (PA.keyword <| toToken "#t")
    , PA.succeed False |. (PA.keyword <| toToken "#f")
    ]

literalParser : T.LiteralType -> Parser c x T.LiteralExpr
literalParser literalType = case literalType of
    T.Bool -> boolParser |> PE.map T.BoolExpr

    T.Int -> PE.numParser "Int" PA.int |> PE.map T.IntExpr

    T.Number -> PE.numParser "Float" PA.float |> PE.map T.NumberExpr

    T.String -> PE.stringParser |> PE.map T.StringExpr

    T.TimestampString -> PE.stringParser |> PE.map T.TimestampExpr

    T.SeriesName -> PE.stringParser |> PE.map T.StringExpr


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

    , PA.succeed T.SeriesName |. PA.keyword (toToken "seriesname") -- py
    , PA.succeed T.SeriesName |. PA.keyword (toToken "SeriesName")
    ]

operatorOutputTypeParser : Parser c x T.OperatorOutputType
operatorOutputTypeParser = asks .expecting <| \expecting ->
    PA.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = expecting "OperatorOutputType"
        }
        |> PA.map T.OperatorOutputType

primitiveTypeParser : Parser c x T.PrimitiveType
primitiveTypeParser = PE.oneOf
    [ literalTypeParser |> PE.map T.Literal
    , operatorOutputTypeParser |> PE.map T.OperatorOutput
    ]

unionTypeParser : Parser c x (NE.Nonempty T.PrimitiveType)
unionTypeParser = ask <| \({toToken, internal} as env) ->
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
    in
    unionTypesParser |> PA.andThen (\xs -> NE.fromList xs |> Maybe.unwrap
        (internal "Empty Union" |> PA.problem)
        (PA.succeed))

parseContainer : T.Key -> Parser c x T.PrimitiveType
parseContainer key = ask <| \({toToken} as env) ->
    PA.succeed identity
        |. PA.keyword (toToken key)
        |. PA.symbol (toToken "[")
        |= Reader.run primitiveTypeParser env
        |. PA.keyword (toToken "]")

argTypeParser : Parser c x T.ArgType
argTypeParser = PE.oneOf
    [ unionTypeParser |> PE.map T.UnionType
    , parseContainer "Packed" |> PE.map (T.Packed >> T.PackedType)
    , primitiveTypeParser |> PE.map T.PrimitiveType
    ]

returnTypeParser : Parser c x (NE.Nonempty T.ReturnType)
returnTypeParser =
    let
        fromArgType argType = asks .internal <| \internal -> case argType of
            T.PrimitiveType x ->
                PA.succeed <| NE.singleton <| T.ReturnPrimitiveType x

            T.UnionType xs ->
                PA.succeed <| NE.map T.ReturnPrimitiveType xs

            T.PackedType _ ->
                PA.problem <| internal "Packed unsupported as return"

    in PE.oneOf
    [ parseContainer "List" |> PE.map (T.ReturnList >> NE.singleton)
    , argTypeParser |> PE.andThen fromArgType
    ]

parseDefaultArgument : Parser c x ( T.ArgType, Maybe T.LiteralExpr )
parseDefaultArgument = ask <| \({toToken} as env) ->
    let
        parseNone = PA.succeed Nothing |. PA.keyword (toToken "None")

        parsePyBool = PA.map (Just << T.BoolExpr) <| PA.oneOf
            [ PA.succeed True |. PA.keyword (toToken "True")
            , PA.succeed False |. PA.keyword (toToken "False")
            ]

    in Reader.run argTypeParser env
        |. PA.symbol (toToken "=")
        |> PA.andThen (\t -> PA.map (Tuple.pair t) <| case t of
            T.PrimitiveType (T.Literal T.Bool) -> parsePyBool

            T.PrimitiveType (T.Literal literalType) -> PA.oneOf
                [ parseNone
                , Reader.run (literalParser literalType) env |> PA.map Just
                ]

            _ -> parseNone)

runParser : String -> Parser Never PE.Problem a -> Either String a
runParser s p =
    let
        showErr err = String.join "\n"
            [ "Error when parsing : " ++ s
            , String.repeat 40 "-"
            , err
            , ""
            ]

    in Either.mapLeft showErr <| PE.run s <| ask <| \({expecting} as env) ->
        Reader.run p env |. (PA.end <| expecting "End of input")

parseArgument : EncodedArg -> Either String Arg
parseArgument {key, encodedValue} =
    runParser encodedValue <| ask <| \({toToken} as env) -> PA.oneOf
        [ PA.succeed (\(argType, defaultExpr) ->
                { isOpt = True
                , key = key
                , argType = argType
                , defaultExpr = defaultExpr
                }
            )
            |. PA.keyword (toToken "Default")
            |. PA.symbol (toToken "[")
            |= Reader.run parseDefaultArgument env
            |. PA.symbol (toToken "]")
        , PA.succeed (\argType ->
                { isOpt = False
                , key = key
                , argType = argType
                , defaultExpr = Nothing
                }
            )
            |= Reader.run argTypeParser env
        ]

parseReturn : EncodedArg -> Either String (NE.Nonempty T.ReturnType)
parseReturn {key, encodedValue} =
    runParser encodedValue <| ask <| \({internal} as env) ->
        if key == "return" then
            Reader.run returnTypeParser env
        else
            PA.problem (internal "No return keyword as first argument")


addArgument : Arg -> T.Operator -> T.Operator
addArgument {isOpt, key, argType, defaultExpr} op =
    if isOpt then
        { op | optArgs = Assoc.insert key (argType, defaultExpr) op.optArgs }
    else
        { op | args = Assoc.insert key argType op.args }

traverseList : (a -> Either String b) -> List a -> Either String (List b)
traverseList f =
    List.foldr (\a b -> Either.map2 (::) (f a) b) (Either.singleton [])

parseOperatorSpec : OperatorSpec -> Either String (NE.Nonempty T.Operator)
parseOperatorSpec {operatorName, encodedArgs} = Either.map2
    (\returnTypes args -> NE.map
        (\returnType -> List.foldl
            addArgument
            (T.Operator operatorName Assoc.empty Assoc.empty returnType)
            args
        )
        returnTypes
    )
    (parseReturn <| NE.head encodedArgs)
    (traverseList parseArgument <| NE.tail encodedArgs)

parseSpec_ : Result JD.Error (List OperatorSpec) -> (SpecErrors, Spec)
parseSpec_ jsonResult =
    let
        addOp op mArray = Just <| Maybe.unwrap
            (Array.fromList [op])
            (Array.push op)
            mArray

        makeSpec : List T.Operator -> Spec
        makeSpec ops = List.foldr
            (\op s -> Assoc.update op.return (addOp op) s)
            Assoc.empty
            ops
            |> Assoc.map (\_ v -> Array.toList v
                |> List.reverse
                |> List.map (\op -> (op.name, op))
                |> Assoc.fromList
            )

        partition :
            List (Either String (NE.Nonempty T.Operator)) -> (SpecErrors, Spec)
        partition = Either.partition >> Tuple.mapBoth
            (NE.fromList)
            (List.concatMap NE.toList >> makeSpec)

    in Either.fromResult jsonResult
        |> Either.mapLeft (JD.errorToString >> NE.singleton >> Just)
        |> Either.unpack
            (\errs -> (errs, Assoc.empty))
            (\ops -> List.map parseOperatorSpec ops |> partition)


type alias UnionTypes = NE.Nonempty T.PrimitiveType

probeUnion : T.ArgType -> List T.PrimitiveType -> T.ArgType
probeUnion argType primitiveTypes = case primitiveTypes of
    [] -> argType

    t :: [] -> T.PrimitiveType t

    t :: xs -> T.UnionType <| NE.Nonempty t xs

reduceForLiteral : UnionTypes -> T.ArgType -> T.ArgType
reduceForLiteral unionTypes argType =
    O.getAll (o OE.neEach_ literal_) unionTypes
        |> List.map T.Literal
        |> probeUnion argType

reduceForOperatorOutput : T.Operator -> UnionTypes -> T.ArgType -> T.ArgType
reduceForOperatorOutput {args} unionTypes argType =
    let
        argsLen = Assoc.size args

        literalNb = List.length <| O.getAll
            (o OE.assocValues_ (o primitiveType_ literal_))
            args

    in if literalNb == (argsLen - 1) then
        O.getAll (o OE.neEach_ operatorOutput_) unionTypes
            |> List.map T.OperatorOutput
            |> probeUnion argType
    else
        argType

reduceUnion : T.Operator -> T.ArgType -> T.ArgType
reduceUnion ({return} as op) argType = case argType of
    T.UnionType primitiveTypes -> case return of
        T.ReturnPrimitiveType (T.Literal _) ->
            reduceForLiteral primitiveTypes argType

        T.ReturnPrimitiveType (T.OperatorOutput _) ->
            reduceForOperatorOutput op primitiveTypes argType

        _ -> argType

    _ -> argType

reduceOperator : T.Operator -> T.Operator
reduceOperator op =
    let
        argTypes_ : O.SimpleTraversal T.Operator T.ArgType
        argTypes_ = o args_ OE.assocValues_

    in if (O.getAll (o argTypes_ union_) op |> List.length) == 1 then
        O.over argTypes_ (reduceUnion op) op
    else
        op

parseSpec :
   T.SpecConfig -> Result JD.Error (List OperatorSpec) -> (SpecErrors, Spec)
parseSpec {reduce} jsonResult =
    parseSpec_ jsonResult |> Tuple.mapSecond (\spec ->
        if reduce then
            Assoc.map
                (\_ ops -> Assoc.map (\_ op -> reduceOperator op) ops)
                spec
        else
            spec
        )

operatorSpecDecoder : JD.Decoder OperatorSpec
operatorSpecDecoder =
    let d = JD.map2 EncodedArg (JD.index 0 JD.string) (JD.index 1 JD.string)
    in JD.map2
        OperatorSpec
        (JD.index 0 JD.string)
        (JD.index 1 <| NEA.decodeArray d)

parseSpecValue : T.SpecConfig -> JD.Value -> (SpecErrors, Spec)
parseSpecValue specCfg specValue =
    JD.decodeValue (JD.list operatorSpecDecoder) specValue
        |> parseSpec specCfg

parseSpecString : T.SpecConfig -> String -> (SpecErrors, Spec)
parseSpecString specConfig specString =
    JD.decodeString (JD.list operatorSpecDecoder) specString
        |> parseSpec specConfig

parseReturnType : String -> Either String T.ReturnType
parseReturnType s =
    runParser s returnTypeParser |> Either.map NE.head


-- optics helpers
args_ : O.SimpleLens ls T.Operator (T.KAssoc T.ArgType)
args_ = O.lens .args <| \s a -> { s | args = a }

primitiveType_ : O.SimplePrism pr T.ArgType T.PrimitiveType
primitiveType_ = O.prism T.PrimitiveType <| \s -> case s of
    T.PrimitiveType xs -> Right xs

    _ -> Left s

union_ : O.SimplePrism pr T.ArgType (NE.Nonempty T.PrimitiveType)
union_ = O.prism T.UnionType <| \s -> case s of
    T.UnionType xs -> Right xs

    _ -> Left s

literal_ : O.SimplePrism pr T.PrimitiveType T.LiteralType
literal_ = O.prism T.Literal <| \s -> case s of
    T.Literal x -> Right x

    _ -> Left s

operatorOutput_ : O.SimplePrism pr T.PrimitiveType T.OperatorOutputType
operatorOutput_ = O.prism T.OperatorOutput <| \s -> case s of
    T.OperatorOutput x -> Right x

    _ -> Left s
