port module FormulaParserValidation.Main exposing (main)

import Either exposing (Either(..))
import Json.Decode as D exposing (Decoder)
import Lazy.Tree.Zipper as Zipper
import Platform exposing (worker)
import TsView.Formula.Parser exposing (parseSpec)
import TsView.Formula.Renderer exposing (renderString)
import TsView.Formula.Spec as S


port log : String -> Cmd msg


type alias Formula =
    { name : String
    , code : String
    }


formulaDecoder : Decoder Formula
formulaDecoder =
    D.map2 Formula (D.field "name" D.string) (D.field "code" D.string)


parseFormula : S.Spec -> String -> Either String String
parseFormula spec formulaCode =
    parseSpec spec formulaCode
        |> Either.map (Zipper.fromTree >> renderString)


parseFormulas : S.Spec -> List Formula -> String
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
                (parseFormula spec formula.code)
    in
    List.foldr
        (\a b -> [ sep ] ++ addEntry a ++ b)
        [ sep ]
        formulas
        |> String.join "\n"


main : Program ( S.JsonSpec, D.Value ) () ()
main =
    let
        run ( jsonSpec, formulasValue ) =
            let
                ( _, spec ) =
                    S.parseJsonSpec jsonSpec

                output =
                    D.decodeValue (D.list formulaDecoder) formulasValue
                        |> Either.fromResult
                        |> Either.map (parseFormulas spec)
                        |> Either.unpack D.errorToString identity
            in
            ( (), log output )
    in
    worker
        { init = run
        , update = \_ () -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
