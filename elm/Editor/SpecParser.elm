module Editor.SpecParser exposing
    ( parseSpecString
    , parseSpecValue
    )

import Either exposing (Either(..))

import AssocList as Assoc
import List.NonEmpty as NE
import Json.Decode as D exposing (Decoder)
import Parser exposing ((|.), (|=), Parser)

import Editor.Type as T
import Editor.Parser exposing (valueParser)


type alias RawOperator =
    ( String, List ( String, String ) )


type alias ParsedArguments =
    { args : List ( String, T.SpecType )
    , optArgs : List ( String, ( T.SpecType, T.EditableValue ) )
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


inputTypeParser : Parser T.LiteralType
inputTypeParser =
    Parser.oneOf
        [ Parser.succeed T.Bool
            |. Parser.keyword "bool"
        , Parser.succeed T.Int
            |. Parser.keyword "int"
        , Parser.succeed T.Number
            |. Parser.keyword "Number"
        , Parser.succeed T.String
            |. Parser.keyword "str"
        , Parser.succeed T.TimestampString
            |. Parser.keyword "Timestamp"
        , Parser.succeed T.SearchString
            |. Parser.keyword "seriesname"
        ]


specTypeParser : Parser T.SpecType
specTypeParser =
    Parser.oneOf
        [ Parser.map T.Editable inputTypeParser
        , Parser.succeed T.Series
            |. Parser.keyword "Series"
        , Parser.succeed T.VarArgs
            |. Parser.keyword "List" -- rename this to Varargs in the Python side
            |. Parser.symbol "["
            |= Parser.lazy (\_ -> specTypeParser)
            |. Parser.symbol "]"
        , Parser.succeed T.Union
            |. Parser.keyword "Union"
            |= (listParser (Parser.lazy (\_ -> specTypeParser))
                    |> Parser.andThen
                        (NE.fromList
                            >> Maybe.map Parser.succeed
                            >> Maybe.withDefault (Parser.problem "Empty Union")
                        )
               )
        ]


parseDefault : Parser ( T.SpecType, T.EditableValue )
parseDefault =
    let
        parseNone =
            Parser.succeed T.Nil |. Parser.keyword "None"

        parseDefaultValue : T.SpecType -> Parser ( T.SpecType, T.EditableValue )
        parseDefaultValue specType =
            Parser.map (Tuple.pair specType) <|
                case specType of
                    T.Editable T.Bool ->
                        Parser.oneOf
                            [ Parser.succeed True |. Parser.keyword "True"
                            , Parser.succeed False |. Parser.keyword "False"
                            ]
                            |> Parser.map T.BoolValue

                    T.Editable x ->
                        Parser.oneOf
                            [ parseNone
                            , valueParser x |> Parser.map Tuple.second
                            ]

                    _ ->
                        parseNone
    in
    specTypeParser
        |. Parser.symbol "="
        |> Parser.andThen parseDefaultValue


parseArgument : ( String, String ) -> ParsedArguments -> ParsedArguments
parseArgument ( name, val ) parsed =
    let
        addOptArg ( x, v ) =
            { parsed | optArgs = ( name, ( x, v ) ) :: parsed.optArgs }

        addArg x =
            { parsed | args = ( name, x ) :: parsed.args }

        parseOptArg =
            Parser.oneOf
                [ Parser.succeed addOptArg
                    |. Parser.keyword "Default"
                    |. Parser.symbol "["
                    |= parseDefault
                    |. Parser.symbol "]"
                , Parser.succeed addArg
                    |= specTypeParser
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


parseOperator : RawOperator -> Either (List String) T.Operator
parseOperator ( name, rawArgs ) =
    let
        opPrefix =
            "Erreur on operator " ++ name ++ " : "

        failOnErrors parsed =
            if List.isEmpty parsed.errors then
                Right parsed

            else
                Left parsed.errors

        makeArgs =
            List.filter (Tuple.first >> (/=) "return") >> Assoc.fromList

        makeOptArgs =
            Assoc.fromList

        makeOperator : ParsedArguments -> Either (List String) T.Operator
        makeOperator parsed =
            let
                partialOp =
                    T.Operator name
                        (makeArgs parsed.args)
                        (makeOptArgs parsed.optArgs)
            in
            Assoc.fromList parsed.args
                |> Assoc.get "return"
                |> Either.fromMaybe [ "No return keyword" ]
                |> Either.map partialOp
    in
    List.foldr parseArgument (ParsedArguments [] [] []) rawArgs
        |> failOnErrors
        |> Either.andThen makeOperator
        |> Either.mapLeft (List.map ((++) opPrefix))


type alias ParsedSpec =
    Either ( T.Spec, NE.NonEmpty String ) T.Spec


parseOperators : List RawOperator -> ParsedSpec
parseOperators ops =
    let
        makeSpec : List T.Operator -> T.Spec
        makeSpec =
            List.map (\x -> ( x.name, x )) >> Assoc.fromList

        parsedOperators : ( List String, T.Spec )
        parsedOperators =
            List.map parseOperator ops
                |> Either.partition
                |> Tuple.mapBoth List.concat makeSpec
    in
    case parsedOperators of
        ( [], s ) ->
            Right s

        ( x :: xs, s ) ->
            Left ( s, NE.fromCons x xs )


parseSpec : Result D.Error (List RawOperator) -> ParsedSpec
parseSpec =
    let
        fail : String -> ( T.Spec, NE.NonEmpty String )
        fail =
            NE.singleton >> Tuple.pair Assoc.empty
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
