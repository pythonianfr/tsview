port module FormulaParserValidation.Main exposing (main)

import Either exposing (Either(..))
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


main : Program ( S.JsonSpec, List Formula ) () ()
main =
    let
        run ( jsonSpec, formulas ) =
            let
                ( _, spec ) =
                    S.parseJsonSpec jsonSpec

                output =
                    parseFormulas spec formulas
            in
            ( (), log output )
    in
    worker
        { init = run
        , update = \_ () -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
