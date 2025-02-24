port module FormulaParserValidation.Main exposing (main)

import Basics.Extra exposing (flip)
import Either exposing (Either)
import Platform exposing (worker)

import Json.Decode as JD
import List.Nonempty as NE

import ParserExtra
import AssocList as Assoc

import Editor.Type as T

import Editor.SpecParser as Parser
import Editor.Parser as Parser

import Editor.SpecRender as SpecRender
import Editor.Render as Render

import Editor.UI.Type as UI
import Editor.UI.Render exposing (renderEditor)


type alias Formula =
    { label : String
    , formulaCode : T.FormulaCode
    }

type alias ArgName =
    { operatorName : String
    , argumentName : String
    }

type RenderOption
    = Inspect
    | Format

type alias Env =
    { spec : T.Spec
    , returnTypeStr : T.ReturnTypeStr
    , renderOption : Maybe RenderOption
    }

parseFormula : Env -> Formula -> String
parseFormula {spec, returnTypeStr, renderOption} {label, formulaCode} =
    Parser.parseFormula spec returnTypeStr formulaCode
        |> ParserExtra.parserErrorsToString
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
                , case renderOption of
                    Just Inspect -> Render.inspectTypedOperator x

                    _ -> Render.renderFormula x
                ])
        |> String.join "\n"

editFormula : Env -> Formula -> String
editFormula {spec, returnTypeStr, renderOption} {label, formulaCode} =
    UI.buildEditor spec returnTypeStr formulaCode |> \editor ->
        [ ""
        , String.repeat 12 " " ++ label
        , ""
        , case renderOption of
            Nothing -> renderEditor editor

            Just Inspect -> UI.renderTypedOperator editor
                |> Render.inspectTypedOperator

            Just Format -> UI.renderTypedOperator editor
                |> Render.renderFormula
        ]
        |> String.join "\n"

renderFormulas : List Formula -> (Formula -> String) -> String
renderFormulas formulas f =
    let sep = String.repeat 80 "-"
    in List.map f formulas
        |> (\xs -> List.concat
            [ List.singleton sep
            , List.intersperse sep xs
            , List.singleton sep
            ])
        |> String.join "\n"

returnTypeDecoder : JD.Decoder T.ReturnTypeStr
returnTypeDecoder = JD.string
    |> JD.index 0
    |> JD.at ["cmd", "contents"]

renderOptionDecoder : JD.Decoder (Maybe RenderOption)
renderOptionDecoder = JD.string
    |> JD.nullable
    |> JD.index 1
    |> JD.at ["cmd", "contents"]
    |> JD.andThen (\s -> case s of
        Nothing -> JD.succeed Nothing

        Just "Inspect" -> JD.succeed <| Just Inspect

        Just "Format" -> JD.succeed <| Just Format

        Just x -> JD.fail <| "Unknow render option : " ++ x)

formulasDecoder : JD.Decoder (List Formula)
formulasDecoder = JD.map2
    Formula
    (JD.field "name" JD.string)
    (JD.field "code" JD.string)
        |> JD.list
        |> JD.index 2
        |> JD.at ["cmd", "contents"]

argNameDecoder = JD.map2
    ArgName
    (JD.index 0 JD.string)
    (JD.index 1 JD.string)
        |> JD.index 1
        |> JD.at ["cmd", "contents"]

processFormulas : T.Spec -> (Env -> Formula -> String) -> JD.Decoder String
processFormulas spec processFormula = JD.map3
    (\returnTypeStr renderOption formulas ->
        Env spec returnTypeStr renderOption
            |> processFormula
            |> renderFormulas formulas
    )
    returnTypeDecoder
    renderOptionDecoder
    formulasDecoder

inspectLiteral : ArgName -> ArgName -> T.PrimitiveExpr -> List String
inspectLiteral userArgName argName primitiveExpr = case primitiveExpr of
    T.LiteralExpr _ (Just expr) ->
        if userArgName == argName then
            [ SpecRender.renderLiteralExpr expr ]
        else
            []

    T.LiteralExpr _ Nothing ->
        []

    T.OperatorExpr typedOp ->
        listLiterals userArgName typedOp 

listLiterals : ArgName -> T.TypedOperator -> List String
listLiterals userArgName {operator, typedArgs, typedOptArgs} =
    let args = Assoc.toList typedArgs ++ Assoc.toList typedOptArgs
    in flip List.concatMap args <| \(name, argExpr) ->
        let argName = ArgName operator.name name
        in case argExpr of
            T.PrimitiveExpr x ->
                inspectLiteral userArgName argName x

            T.UnionExpr _ (_, x) ->
                inspectLiteral userArgName argName x

            T.VarArgsExpr _ xs ->
                List.concatMap (inspectLiteral userArgName argName) xs

            T.PackedExpr _ typedOp ->
                listLiterals userArgName typedOp

parseFormula_ :
   T.Spec -> T.ReturnTypeStr -> Formula -> Either String T.TypedOperator
parseFormula_ spec returnTypeStr {label, formulaCode} =
    Parser.parseFormula spec returnTypeStr formulaCode
        |> ParserExtra.parserErrorsToString
        |> Either.mapLeft (\x -> "ERROR on " ++ label ++ " : " ++ x)

traverseEither_ : (x -> Either e a) -> List x -> Either e (List a)
traverseEither_ f xs =
    List.foldr (\a b -> Either.map2 (::) (f a) b) (Either.singleton []) xs

processLiterals : T.Spec -> JD.Decoder String
processLiterals spec = JD.map3
    (\returnTypeStr argName formulas ->
        traverseEither_ (parseFormula_ spec returnTypeStr) formulas
            |> Either.map (List.concatMap (listLiterals argName))
            |> Either.unpack identity (String.join "\n")
    )
    returnTypeDecoder
    argNameDecoder
    formulasDecoder

cmdDecoder : ( T.Spec, String ) -> JD.Decoder String
cmdDecoder ( spec, cmd ) = case cmd of
    "ShowSpec" -> JD.succeed <| SpecRender.renderSpec spec

    "Parse" -> processFormulas spec parseFormula

    "Edit" -> processFormulas spec editFormula

    "ListLiterals" -> processLiterals spec 

    x -> JD.fail <| "Unknow command : " ++ x

specDecoder_ : (Bool, JD.Value) -> JD.Decoder T.Spec
specDecoder_ (doReduce, jsonValue) =
    case Parser.parseSpecValue {reduce = doReduce} jsonValue of
        (Nothing, spec) -> spec
            |> JD.succeed

        (Just errs, _ ) -> NE.toList errs
            |> String.join "\n"
            |> JD.fail

specDecoder : JD.Decoder T.Spec
specDecoder = JD.map2
    Tuple.pair
    (JD.field "reduceSpec" JD.bool)
    (JD.field "spec" JD.value)
        |> JD.andThen specDecoder_

run : JD.Value -> String
run jsonValue = JD.map2
    Tuple.pair
    (specDecoder)
    (JD.at ["cmd", "tag"] JD.string)
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
