module Editor.Parser exposing (..)

import Set
import Dict
import Tuple.Extra as Tuple
import Maybe.Extra as Maybe
import Either exposing (Either)

import Reader
import List.Extra
import List.Nonempty as NE
import Parser.Advanced as PA exposing ((|.), (|=))

import AssocList as Assoc
import ParserExtra as PE
import ReaderExtra exposing (ask, asks, askM, asksM)

import Editor.Type as T
import Editor.SpecParser as SpecParser
import Editor.SpecRender exposing (findOperator, renderArgType)


type alias SpecEnv =
    { spec : T.Spec
    , returnType : T.ReturnType
    , operator : T.Operator
    }

type Ctx = OperatorCtx String

type alias CParser x a =
    Reader.Reader SpecEnv (PE.Parser Ctx x a)


map : (a -> b) -> CParser x a -> CParser x b
map f = Reader.map (PE.map f)

andThen : (a -> CParser x b) -> CParser x a -> CParser x b
andThen f ma =
    let andThen_ : SpecEnv -> PE.Parser Ctx x b
        andThen_ env =
            Reader.run ma env |> PE.andThen (\a -> Reader.run (f a) env)
    in Reader.asks andThen_

evalParser :
    SpecEnv -> PE.ProblemMap Ctx x -> CParser x a -> PA.Parser Ctx x a
evalParser specEnv problemMap pa =
    Reader.run (Reader.run pa specEnv) problemMap

oneOf : List (CParser x a) -> CParser x a
oneOf xs =
    ask <| \specEnv ->
        ask <| \problemMap ->
            PA.oneOf (List.map (evalParser specEnv problemMap) xs)

many :  CParser x a -> CParser x (List a)
many pa =
    ask <| \specEnv ->
        ask <| \problemMap ->
            PE.many (evalParser specEnv problemMap pa)

succeed : a -> CParser x a
succeed x =
    Reader.reader <|
        Reader.reader <|
            PA.succeed x

problem : String -> CParser x a
problem s =
    Reader.reader <|
        asks .invalid <| \invalid ->
            PA.problem <| invalid s

inContext : Ctx -> CParser x a -> CParser x a
inContext ctx pa =
    ask <| \specEnv ->
        ask <| \problemMap ->
            PA.inContext ctx (evalParser specEnv problemMap pa)


parseLiteralExpr : T.LiteralType -> CParser x T.PrimitiveExpr
parseLiteralExpr t = oneOf
    -- XXX should handle nil
    [ Reader.reader (SpecParser.literalParser t)
        |> map (Just >> T.LiteralExpr t)
    , parseTypedOperator_ |> map T.OperatorExpr
    ]

setReturnType : T.PrimitiveType -> CParser x a -> CParser x a
setReturnType p =
    let t = T.ReturnPrimitiveType p
    in Reader.local (\senv -> {senv | returnType = t})

parsePrimitiveExpr : T.PrimitiveType -> CParser x T.PrimitiveExpr
parsePrimitiveExpr p = setReturnType p <| case p of
    T.Literal t -> parseLiteralExpr t

    T.OperatorOutput _ -> parseTypedOperator |> map T.OperatorExpr

parseUnion :
    NE.Nonempty T.PrimitiveType ->
    CParser x (T.PrimitiveType, T.PrimitiveExpr)
parseUnion primitiveTypes =
    NE.toList primitiveTypes
        |> List.map (\x -> map (Tuple.pair x) (parsePrimitiveExpr x))
        |> oneOf
        -- XXX show problems

parsePacked : T.Packed -> CParser x T.TypedOperator
parsePacked (T.Packed t) =
    let x = T.ReturnList t
    in Reader.local (\senv -> {senv | returnType = x}) parseTypedOperator_

parseVarArgs : T.Packed -> CParser x (List T.PrimitiveExpr)
parseVarArgs (T.Packed t) =
    many (parsePrimitiveExpr t)

parseArgExpr : T.ArgType -> CParser x T.ArgExpr
parseArgExpr argType = case argType of
    T.PrimitiveType t ->
        parsePrimitiveExpr t |> map T.PrimitiveExpr

    T.UnionType xs ->
        parseUnion xs |> map (T.UnionExpr xs)

    T.PackedType p -> oneOf
        [ parsePacked p |> map (T.PackedExpr p)
        , parseVarArgs p |> map (T.VarArgsExpr p)
        ]

parseArg : Bool -> (T.Key, T.ArgType) -> CParser x (T.Key, T.ArgExpr)
parseArg isKeyword (k, t) =
    ask <| \senv ->
        ask <| \({toToken} as pm) ->
            let
                parseKeyword = PA.keyword (toToken <| "#:" ++ k)
            in
            PA.succeed (Tuple.pair k)
                |. (if isKeyword then parseKeyword else PE.empty)
                |. PA.spaces
                |= evalParser senv pm (parseArgExpr t)

parsePositional : (T.Key, T.ArgType) -> CParser x (T.Key, T.ArgExpr)
parsePositional = parseArg False

parseWithKey : (T.Key, T.ArgType) -> CParser x (T.Key, T.ArgExpr)
parseWithKey = parseArg True


type alias ArgsSpec =
    List (T.Key, T.ArgType)


type alias Args =
    List (T.Key, T.ArgExpr)


parseArgs_ :
    SpecEnv ->
    PE.ProblemMap Ctx x ->
    (ArgsSpec,  Args) ->
    PA.Parser Ctx x (PA.Step (ArgsSpec, Args) Args)
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

parseArgs : ArgsSpec -> Args -> CParser x Args
parseArgs argsSpec args =
    ask <| \senv ->
        ask <| \pm ->
            PA.loop (argsSpec, args) (parseArgs_ senv pm)

parseArgsWithKey : ArgsSpec -> CParser x Args
parseArgsWithKey argsSpec = asksM (.operator >> .optArgs) <| \optArgs ->
    let
        spec = Assoc.toList optArgs
            |> List.map (Tuple.mapSecond Tuple.first) -- drop default value
            |> List.append argsSpec

    in many <| oneOf (List.map parseWithKey spec)

parseTypedArgs : T.TypedOperator -> CParser x T.TypedOperator
parseTypedArgs ({operator} as op) =
    let
        argKeys = Assoc.keys operator.args
        optArgKeys = Assoc.keys operator.optArgs

        specKeyIdx : Dict.Dict T.Key Int
        specKeyIdx =
            List.indexedMap Tuple.pair (argKeys ++ optArgKeys)
                |> List.map Tuple.flip
                |> Dict.fromList

        sortBySpecKeys : Args -> T.ArgExprs
        sortBySpecKeys xs = Assoc.fromList <| List.sortBy
            (\( k, _ ) -> Dict.get k specKeyIdx |> Maybe.withDefault -1)
            xs

        checkDuplicate : Args -> CParser x Args
        checkDuplicate xs =
            if List.Extra.allDifferentBy Tuple.first xs then
                succeed xs
            else
                problem "Duplicate keyword for argument"

        renderArg : T.ArgType -> String
        renderArg t =
            "Lack of a " ++ renderArgType t ++ " mandatory argument"

        checkArgs : T.TypedOperator -> CParser x T.TypedOperator
        checkArgs ({ typedArgs } as t) =
            let typedArgsKeys = Assoc.keys typedArgs
            in List.filter
                (\(k, _) -> not <| List.member k typedArgsKeys)
                (Assoc.toList operator.args)
                |> List.head
                |> Maybe.unwrap
                    (succeed t)
                    (Tuple.second >> renderArg >> problem)

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

parseOperatorName : CParser x String
parseOperatorName = Reader.reader <| ask <| \{expecting} ->
    let specials = String.toList "_-+*/.<>="
        accept = \c -> List.member c specials
    in PA.variable
        { start = \c -> Char.isLower c || accept c
        , inner = \c -> Char.isAlphaNum c || accept c
        , reserved = Set.empty
        , expecting = expecting "Valid operator name"
        }
        |. PA.spaces

getOperator : CParser x T.TypedOperator
getOperator = askM <| \({spec, returnType}) ->
    let initTyped op = T.TypedOperator op Assoc.empty Assoc.empty
    in parseOperatorName
        |> andThen (findOperator spec returnType
            >> Either.unpack problem (initTyped >> succeed)
        )

parseTypedOperator_ : CParser x T.TypedOperator
parseTypedOperator_ =
    ask <| \senv ->
        ask <| \pm ->
            PA.backtrackable (evalParser senv pm parseTypedOperator)

parseTypedOperator : CParser x T.TypedOperator
parseTypedOperator =
    ask <| \senv ->
        ask <| \({toToken} as pm) -> getOperator
            |> andThen (\typedOp -> inContext
                (OperatorCtx typedOp.operator.name)
                (parseTypedArgs typedOp))
            |> evalParser senv pm
            |> PE.between
                (PA.symbol (toToken "(")
                    |. PA.spaces)
                (PA.spaces
                    |. PA.symbol (toToken ")"))

parseTopOperator : CParser x T.TypedOperator
parseTopOperator =
    ask <| \senv ->
        ask <| \({expecting} as pm) ->
            PA.succeed identity
                |. PA.spaces
                |= (evalParser senv pm parseTypedOperator)
                |. PA.spaces
                |. (PA.end <| expecting "End of formula")

renderCtx : Ctx -> String
renderCtx ctx = case ctx of
    OperatorCtx name -> "inside operator " ++ name

parseFormula :
    T.Spec ->
    T.ReturnTypeStr ->
    T.FormulaCode ->
    Either PE.ParserErrors T.TypedOperator
parseFormula spec returnTypeStr formulaCode =
    SpecParser.parseReturnType returnTypeStr
        |> Either.mapLeft (PE.toParserError >> NE.singleton)
        |> Either.andThen (\returnType ->
        -- SpecEnv (voidOperator will disappear in parseTypedOperator)
        { spec = spec
        , returnType = returnType
        , operator = T.voidOperator
        }
            |> Reader.run parseTopOperator
            |> PE.runParser
                (PE.defaultProblemMap renderCtx)
                formulaCode
        )
