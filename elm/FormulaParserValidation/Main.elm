port module FormulaParserValidation.Main exposing (main)

import Either
import Platform exposing (worker)

import Json.Decode as JD
import List.NonEmpty as NE

import Editor.Type as T

import Editor.SpecParser as Parser
import Editor.Parser as Parser

import Editor.SpecRender as Render
import Editor.Render as Render


type alias Formula =
    { label : String
    , formulaCode : T.FormulaCode
    }

parseFormula : T.GSpec -> Formula -> String
parseFormula gSpec {label, formulaCode} =
    Parser.parseFormula gSpec T.returnSeries formulaCode
        |> Parser.parserErrorsToString
        |> Either.unpack
            (\x ->
                [ label ++ " [KO]"
                , ""
                ,formulaCode
                , ""
                , x
                ])
            (\x ->
                [ label ++ " [OK]"
                , ""
                , Render.renderFormula x
                ])
        |> String.join "\n"

parseFormulas : T.GSpec -> List Formula -> String
parseFormulas gSpec formulas =
    let sep = String.repeat 80 "-"
    in List.map (parseFormula gSpec) formulas
        |> (\xs -> List.concat
            [ List.singleton sep
            , List.intersperse sep xs
            , List.singleton sep
            ])
        |> String.join "\n"

formulaDecoder : JD.Decoder Formula
formulaDecoder =
    JD.map2
        Formula
        (JD.field "name" JD.string)
        (JD.field "formula" JD.string)

cmdDecoder : ( T.GSpec, String ) -> JD.Decoder String
cmdDecoder ( gSpec, cmd ) = case cmd of
    "Check" ->
        JD.succeed <| Render.renderGSpec gSpec

    "Parse" ->
        JD.field "formulas" (JD.list formulaDecoder)
            |> JD.map (parseFormulas gSpec)

    x ->
        JD.fail <| "Unknow command : " ++ x

specDecoder : JD.Value -> JD.Decoder T.GSpec
specDecoder jsonValue = case Parser.parseSpecValue jsonValue of
    (Nothing, spec) -> T.buildGSpec spec
        |> JD.succeed

    (Just errs, _ ) -> NE.toList errs
        |> String.join "\n"
        |> JD.fail

run : JD.Value -> String
run jsonValue = JD.map2
    Tuple.pair
    (JD.field "spec" JD.value |> JD.andThen specDecoder)
    (JD.field "cmd" JD.string)
        |> JD.andThen cmdDecoder
        |> (\decoder -> JD.decodeValue decoder jsonValue)
        |> Either.fromResult
        |> Either.unpack JD.errorToString identity


port log : String -> Cmd msg


main : Program JD.Value () ()
main = worker
    { init = run >> log >> Tuple.pair ()
    , update = \_ () -> ( (), Cmd.none )
    , subscriptions = \_ -> Sub.none
    }
