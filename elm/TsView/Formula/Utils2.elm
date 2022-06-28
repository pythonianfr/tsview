module TsView.Formula.Utils2 exposing (..)

import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import Set
import TsView.Formula.Type as T


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


valueParser : T.InputType -> Parser ( String, T.Value )
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

        T.Timestamp ->
            -- XXX should be a date parser ?
            stringParser
                |> andThen (\x -> ( x, T.TimestampValue x ))

        T.SearchString ->
            stringParser
                |> andThen (\x -> ( x, T.StringValue x ))


strInputType : T.InputType -> String
strInputType iType =
    case iType of
        T.Int ->
            "Int"

        T.Number ->
            "Number"

        T.String ->
            "String"

        T.Bool ->
            "Bool"

        T.Timestamp ->
            "Timestamp"

        T.SearchString ->
            "SearchString"


strSpecType : T.SpecType -> String
strSpecType sType =
    case sType of
        T.BaseInput x ->
            strInputType x

        T.Series ->
            "Series"

        T.List x ->
            "List[" ++ strSpecType x ++ "]"

        T.Union xs ->
            let
                typesToStr =
                    NE.map strSpecType >> NE.toList >> String.join ", "
            in
            "Union[" ++ typesToStr xs ++ "]"
