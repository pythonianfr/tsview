module TsView.Formula.Spec.Parser exposing
    ( parseSpecString
    , parseSpecValue
    )

import AssocList as Assoc
import Either exposing (Either(..))
import Json.Decode as D exposing (Decoder)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import TsView.Formula.Parser exposing (stringParser)
import TsView.Formula.Spec.Type as S


type alias RawOperator =
    ( String, List ( String, String ) )


type alias ParsedArguments =
    { args : List ( String, S.ExpType )
    , kargs : List ( String, S.ExpType )
    , errors : List String
    }


listParser : Parser a -> Parser (List a)
listParser itemParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = itemParser
        , trailing = Parser.Forbidden
        }


inputTypeParser : Parser S.InputType
inputTypeParser =
    Parser.oneOf
        [ Parser.succeed S.Int
            |. Parser.keyword "int"
        , Parser.succeed S.Number
            |. Parser.keyword "Number"
        , Parser.succeed S.String
            |. Parser.keyword "str"
        , Parser.succeed S.Bool
            |. Parser.keyword "bool"
        , Parser.succeed S.Timestamp
            |. Parser.keyword "Timestamp"
        , Parser.succeed S.SearchString
            |. Parser.keyword "search_str"
        ]


baseTypeParser : Parser S.BaseType
baseTypeParser =
    Parser.oneOf
        [ Parser.map S.BaseInput inputTypeParser
        , Parser.succeed S.Series
            |. Parser.keyword "Series"
        ]


expTypeParser : Parser S.ExpType
expTypeParser =
    Parser.oneOf
        [ Parser.map S.ExpBaseType baseTypeParser
        , Parser.succeed S.SList
            |. Parser.keyword "List"
            |. Parser.symbol "["
            |= Parser.lazy (\_ -> expTypeParser)
            |. Parser.symbol "]"
        , Parser.succeed S.Union
            |. Parser.keyword "Union"
            |= (listParser (Parser.lazy (\_ -> expTypeParser))
                    |> Parser.andThen
                        (NE.fromList
                            >> Maybe.map Parser.succeed
                            >> Maybe.withDefault (Parser.problem "Empty Union")
                        )
               )
        ]


parseArgument : ( String, String ) -> ParsedArguments -> ParsedArguments
parseArgument ( name, val ) parsed =
    let
        addKArg x =
            { parsed | kargs = ( name, x ) :: parsed.kargs }

        addArg x =
            { parsed | args = ( name, x ) :: parsed.args }

        parseOptArg =
            Parser.oneOf
                [ Parser.succeed addKArg
                    |. Parser.keyword "Optional"
                    |. Parser.symbol "["
                    |= expTypeParser
                    |. Parser.symbol "]"
                , Parser.succeed addArg
                    |= expTypeParser
                ]

        errName =
            (++) (name ++ " ")
    in
    Parser.run (parseOptArg |. Parser.end) val
        |> Either.fromResult
        |> Either.mapLeft (Parser.deadEndsToString >> errName)
        |> Either.unpack
            (\x -> { parsed | errors = x :: parsed.errors })
            identity


parseOperator : RawOperator -> Either (List String) S.Operator
parseOperator ( name, rawArgs ) =
    let
        failOnErrors parsed =
            if List.isEmpty parsed.errors then
                Right parsed

            else
                Left parsed.errors

        validArgs =
            List.filter (Tuple.first >> (/=) "return") >> List.map Tuple.second

        makeOperator parsed =
            Assoc.fromList parsed.args
                |> Assoc.get "return"
                |> Either.fromMaybe [ "No return keyword" ]
                |> Either.map (S.Operator name (validArgs parsed.args) parsed.kargs)
    in
    List.foldr parseArgument (ParsedArguments [] [] []) rawArgs
        |> failOnErrors
        |> Either.andThen makeOperator


type alias ParsedSpec =
    Either ( S.Spec, Nonempty String ) S.Spec


listBaseTypes : S.ExpType -> Nonempty S.BaseType
listBaseTypes expType =
    case expType of
        S.ExpBaseType x ->
            NE.fromElement x

        S.SList x ->
            listBaseTypes x

        S.Union xs ->
            NE.concat <| NE.map listBaseTypes xs


groupOperators : List S.Operator -> S.Spec
groupOperators ops =
    let
        addOperator op xs =
            Maybe.map (NE.cons op) xs
                |> Maybe.withDefault (NE.fromElement op)
                |> Just

        group op store =
            List.foldr
                (\baseType b -> Assoc.update baseType (addOperator op) b)
                store
                (listBaseTypes op.return |> NE.toList)
    in
    List.foldr group Assoc.empty ops


parseOperators : List RawOperator -> ParsedSpec
parseOperators ops =
    let
        parsedOperators : ( List String, S.Spec )
        parsedOperators =
            List.map parseOperator ops
                |> Either.partition
                |> Tuple.mapBoth List.concat groupOperators
    in
    case parsedOperators of
        ( [], s ) ->
            Right s

        ( x :: xs, s ) ->
            Left ( s, NE.Nonempty x xs )


parseSpec : Result D.Error (List RawOperator) -> ParsedSpec
parseSpec =
    let
        fail : String -> ( S.Spec, Nonempty String )
        fail =
            NE.fromElement >> Tuple.pair S.noSpec
    in
    Either.fromResult
        >> Either.mapLeft (D.errorToString >> fail)
        >> Either.andThen parseOperators


tupleDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
tupleDecoder da db =
    D.map2 Tuple.pair (D.index 0 da) (D.index 1 db)


rawOperatorDecoder : Decoder RawOperator
rawOperatorDecoder =
    tupleDecoder
        D.string
        (D.list (tupleDecoder D.string D.string))


parseSpecValue : D.Value -> ParsedSpec
parseSpecValue =
    D.decodeValue (D.list rawOperatorDecoder) >> parseSpec


parseSpecString : String -> ParsedSpec
parseSpecString =
    D.decodeString (D.list rawOperatorDecoder) >> parseSpec
