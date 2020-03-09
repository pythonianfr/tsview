port module FormulaParserValidation.Main exposing (main)

import Either exposing (Either(..))
import Json.Decode as D exposing (Decoder)
import Lazy.Tree.Zipper as Zipper
import List.Nonempty as NE exposing (Nonempty)
import Platform exposing (worker)
import TsView.Formula.EditionTree.Inspect exposing (inspectEditionTree)
import TsView.Formula.EditionTree.Parser exposing (parseFormula)
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.Spec.Parser exposing (parseSpecValue)
import TsView.Formula.Spec.Render as SR exposing (renderSpec)
import TsView.Formula.Spec.Type as S exposing (Spec)


check : Spec -> String
check =
    renderSpec


inspect : Spec -> Bool -> String
inspect spec all =
    let
        sep =
            [ String.repeat 5 "=" ]

        renderOps =
            NE.toList
                >> List.map
                    (ET.buildEditionTree spec >> inspectEditionTree)

        render : ( S.BaseType, Nonempty ET.EditionType ) -> List String
        render ( baseType, ops ) =
            [ SR.strBaseType baseType, String.repeat 5 "-" ] ++ renderOps ops
    in
    if all then
        List.foldl (\a b -> b ++ render a) sep (ET.specToList spec)
            |> String.join "\n"

    else
        ET.buildInitialTree spec |> inspectEditionTree


type alias Formula =
    { name : String
    , code : String
    }


formulaDecoder : Decoder Formula
formulaDecoder =
    D.map2 Formula (D.field "name" D.string) (D.field "code" D.string)


parseFormulas : Spec -> List Formula -> String
parseFormulas spec formulas =
    let
        sep =
            String.repeat 80 "-"

        addEntry formula =
            let
                rdr label content =
                    [ formula.name ++ " [" ++ label ++ "]", content ]
            in
            Either.unpack
                (\x -> rdr "ERROR" x ++ [ formula.code ])
                (rdr "OK")
                (parseFormula spec formula.code |> Either.map inspectEditionTree)
    in
    List.foldl
        (\a b -> [ sep ] ++ addEntry a ++ b)
        [ sep ]
        formulas
        |> List.reverse
        |> String.join "\n"


specDecoder : D.Value -> Decoder Spec
specDecoder =
    let
        errMess =
            Tuple.second >> NE.toList >> String.join "\n" >> D.fail
    in
    parseSpecValue >> Either.unpack errMess D.succeed


cmdDecoder : ( Spec, String ) -> Decoder String
cmdDecoder ( spec, cmd ) =
    case cmd of
        "Check" ->
            D.succeed (check spec)

        "Inspect" ->
            D.map (inspect spec) (D.field "all" D.bool)

        "Parse" ->
            D.map
                (parseFormulas spec)
                (D.field "formulas" (D.list formulaDecoder))

        x ->
            D.fail ("Unknow command : " ++ x)


run : D.Value -> String
run =
    let
        dec =
            D.map2
                Tuple.pair
                (D.field "spec" D.value |> D.andThen specDecoder)
                (D.field "cmd" D.string)
    in
    D.decodeValue (dec |> D.andThen cmdDecoder)
        >> Either.fromResult
        >> Either.unpack D.errorToString identity


port log : String -> Cmd msg


main : Program D.Value () ()
main =
    worker
        { init = run >> log >> Tuple.pair ()
        , update = \_ () -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
