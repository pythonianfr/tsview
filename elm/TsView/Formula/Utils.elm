module TsView.Formula.Utils exposing
    ( buildForest
    , numberParser
    , stringParser
    )

import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import Parser exposing ((|.), (|=), Parser)
import Set


{-| Build a `Tree.Forest b` (aka `LList Tree b`) from `Zipper` children
-}
buildForest : (Zipper a -> Tree b) -> Zipper a -> Tree.Forest b
buildForest mkTree =
    Zipper.openAll >> List.map mkTree >> LL.fromList


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
