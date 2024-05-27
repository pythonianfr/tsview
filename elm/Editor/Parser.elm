module Editor.Parser exposing (..)

import Set
import Dict
import Tuple.Extra as Tuple
import Maybe.Extra as Maybe
import Either exposing (Either)

import Reader
import ReaderExtra exposing (ask, asks, askM, asksM)
import List.Extra
import AssocList as Assoc
import List.Nonempty as NE
import Parser.Advanced as PA exposing ((|.), (|=))

import Editor.Type as T
import Editor.SpecParser as Parser exposing (Parser)


type alias SpecEnv =
    { gSpec : T.GSpec
    , returnType : T.ReturnType
    , operator : T.Operator
    }


type alias Parser c x a =
    Reader.Reader SpecEnv (Parser.Parser c x a)


map : (a -> b) -> Parser c x a -> Parser c x b
map f = Reader.map (Parser.map f)

andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen f ma =
    let andThen_ : SpecEnv -> Parser.Parser c x b
        andThen_ env =
            Reader.run ma env |> Parser.andThen (\a -> Reader.run (f a) env)
    in Reader.asks andThen_

evalParser :
    SpecEnv -> Parser.ProblemMap c x -> Parser c x a -> PA.Parser c x a
evalParser specEnv problemMap pa =
    Reader.run (Reader.run pa specEnv) problemMap

oneOf : List (Parser c x a) -> Parser c x a
oneOf xs =
    ask <| \specEnv ->
        ask <| \problemMap ->
            PA.oneOf (List.map (evalParser specEnv problemMap) xs)

many :  Parser c x a -> Parser c x (List a)
many pa =
    ask <| \specEnv ->
        ask <| \problemMap ->
            Parser.many (evalParser specEnv problemMap pa)

succeed : a -> Parser c x a
succeed x =
    Reader.reader <|
        Reader.reader <|
            PA.succeed x

problem : String -> Parser c x a
problem s =
    Reader.reader <|
        asks .internal <| \internal ->
            PA.problem <| internal s


parseLiteralExpr : T.LiteralType -> Parser c x T.PrimitiveExpr
parseLiteralExpr t = oneOf
    -- XXX should handle nil
    [ Reader.reader (Parser.literalParser t) |> map (Just >> T.LiteralExpr t)
    , parseTypedOperator |> map T.OperatorExpr
    ]

setReturnType : T.PrimitiveType -> Parser c x a -> Parser c x a
setReturnType p =
    let t = T.BaseReturnType <| T.findBaseReturnType p
    in Reader.local (\senv -> {senv | returnType = t})

parsePrimitiveExpr : T.PrimitiveType -> Parser c x T.PrimitiveExpr
parsePrimitiveExpr p = setReturnType p <| case p of
    T.Literal t -> parseLiteralExpr t

    T.OperatorOutput _ -> parseTypedOperator |> map T.OperatorExpr

    T.Union xs -> parseUnion xs |> map (T.UnionExpr xs)

parseUnion :
    NE.Nonempty T.PrimitiveType ->
    Parser c x (T.PrimitiveType, T.PrimitiveExpr)
parseUnion primitiveTypes =
    NE.toList primitiveTypes
        |> List.map (\x -> map (Tuple.pair x) (parsePrimitiveExpr x))
        |> oneOf
        -- XXX show problems

parsePacked : T.PrimitiveType -> Parser c x T.TypedOperator
parsePacked t =
    let x = T.ReturnPacked <| T.listBaseReturnType t
    in Reader.local (\senv -> {senv | returnType = x}) parseTypedOperator

parseVarArgs : T.PrimitiveType -> Parser c x (List T.PrimitiveExpr)
parseVarArgs t =
    many (parsePrimitiveExpr t)

parseTypedExpr : T.SpecType -> Parser c x T.TypedExpr
parseTypedExpr specType = case specType of
    T.PrimitiveType t ->
        parsePrimitiveExpr t |> map T.PrimitiveExpr

    T.CompositeType (T.VarArgs t) ->
        parseVarArgs t |> map (T.VarArgsExpr t >> T.CompositeExpr)

    T.CompositeType (T.Packed _) ->
        problem "Packed unsupported as argument type"

parseArg : Bool -> (T.Key, T.SpecType) -> Parser c x (T.Key, T.TypedExpr)
parseArg isKeyword (k, t) =
    ask <| \senv ->
        ask <| \({toToken} as pm) ->
            let
                parseKeyword = PA.keyword (toToken <| "#:" ++ k)
            in
            PA.succeed (Tuple.pair k)
                |. (if isKeyword then parseKeyword else Parser.empty)
                |. PA.spaces
                |= evalParser senv pm (parseTypedExpr t)

parsePositional : (T.Key, T.SpecType) -> Parser c x (T.Key, T.TypedExpr)
parsePositional = parseArg False

parseWithKey : (T.Key, T.SpecType) -> Parser c x (T.Key, T.TypedExpr)
parseWithKey = parseArg True


type alias ArgsSpec =
    List (T.Key, T.SpecType)


type alias Args =
    List (T.Key, T.TypedExpr)


parseArgs_ :
    SpecEnv ->
    Parser.ProblemMap c x ->
    (ArgsSpec,  Args) ->
    PA.Parser c x (PA.Step (ArgsSpec, Args) Args)
parseArgs_ senv pm (argsSpec, args) =
    let
        done = (parseArgsWithKey argsSpec |> evalParser senv pm)
            |> PA.map (List.append (List.reverse args) >> PA.Done)

    in case argsSpec of
        x :: xs -> PA.oneOf
            [ (parsePositional x |> evalParser senv pm)
                |. PA.spaces
                |> PA.map (\a -> PA.Loop (xs, a::args))
            , done
            ]

        [] -> done

parseArgs : ArgsSpec -> Args -> Parser c x Args
parseArgs argsSpec args =
    ask <| \senv ->
        ask <| \pm ->
            PA.loop (argsSpec, args) (parseArgs_ senv pm)

parseArgsWithKey : ArgsSpec -> Parser c x Args
parseArgsWithKey argsSpec = asksM (.operator >> .optArgs) <| \optArgs ->
    let
        spec = Assoc.toList optArgs
            |> List.map (Tuple.mapSecond Tuple.first) -- drop default value
            |> List.append argsSpec

    in many <| oneOf (List.map parseWithKey spec)

parseTypedArgs : T.TypedOperator -> Parser c x T.TypedOperator
parseTypedArgs ({operator} as op) =
    let
        argKeys = Assoc.keys operator.args
        optArgKeys = Assoc.keys operator.optArgs

        specKeyIdx : Dict.Dict T.Key Int
        specKeyIdx =
            List.indexedMap Tuple.pair (argKeys ++ optArgKeys)
                |> List.map Tuple.flip
                |> Dict.fromList

        sortBySpecKeys : Args -> T.TypedExprs
        sortBySpecKeys xs = Assoc.fromList <| List.sortBy
            (\( k, _ ) -> Dict.get k specKeyIdx |> Maybe.withDefault -1)
            xs

        checkDuplicate : Args -> Parser c x Args
        checkDuplicate xs =
            if List.Extra.allDifferentBy Tuple.first xs then
                succeed xs
            else
                problem "Duplicate keyword for argument"

        checkArgs : T.TypedOperator -> Parser c x T.TypedOperator
        checkArgs ({ typedArgs, typedOptArgs } as t) =
            if Assoc.size typedArgs == List.length argKeys then
                succeed t
            else
                problem "Lack of mandatory argument"

    in
    Reader.local
        (\senv -> {senv | operator = operator})
        (parseArgs (Assoc.toList operator.args) [])
        |> andThen checkDuplicate
        |> map sortBySpecKeys -- order user input from Spec for rendering
        |> map (Assoc.partition (\k _ -> List.member k argKeys))
        |> map (\(typedArgs, typedOptArgs) ->
            {op | typedArgs = typedArgs, typedOptArgs = typedOptArgs})
        |> andThen checkArgs

parseOperatorName : Parser c x String
parseOperatorName = Reader.reader <| asks .expecting <| \expecting ->
    let specials = String.toList "_-+*/.<>="
        accept = \c -> List.member c specials
    in PA.variable
        { start = \c -> Char.isLower c || accept c
        , inner = \c -> Char.isAlphaNum c || accept c
        , reserved = Set.empty
        , expecting = expecting "Valid operator name"
        }
        |. PA.spaces

getOperator : Parser c x (T.TypedOperator)
getOperator = askM <| \({gSpec, returnType}) ->
    let initTyped op = T.TypedOperator op Assoc.empty Assoc.empty returnType
    in parseOperatorName
        |>  map 
            (\k -> Assoc.get k gSpec.spec |> Either.fromMaybe ("no " ++ k))
        |> andThen (Either.unpack problem (initTyped >> succeed))

parseTypedOperator : Parser c x T.TypedOperator
parseTypedOperator =
    ask <| \senv ->
        ask <| \({toToken} as pm) -> Parser.between
            (PA.symbol (toToken "(") |. PA.spaces)
            (PA.spaces |. PA.symbol (toToken ")"))
            (getOperator |> andThen parseTypedArgs |> evalParser senv pm)

parseTopOperator : Parser c x T.TypedOperator
parseTopOperator =
    ask <| \senv ->
        ask <| \({expecting} as pm) ->
            PA.succeed identity
                |. PA.spaces
                |= (evalParser senv pm parseTypedOperator)
                |. PA.spaces
                |. (PA.end <| expecting "End of formula")

parseFormula :
    T.GSpec ->
    T.ReturnType ->
    T.FormulaCode ->
    Either T.ParserErrors T.TypedOperator
parseFormula gSpec returnType formulaCode =
    { gSpec = gSpec
    , returnType = returnType
    , operator = T.voidOperator
    }
        |> Reader.run parseTopOperator
        |> Parser.runParser
            (Parser.defaultProblemMap <| always "C")
            formulaCode
