module TsView.Formula.Utils exposing
    ( boolParser
    , boolToString
    , buildForest
    , buildTree
    , numberParser
    , stringParser
    )

import Parser exposing ((|.), (|=), Parser)
import Set
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


buildTree : (a -> List a) -> a -> Tree a
buildTree getChildren parent =
    case getChildren parent of
        [] ->
            Tree.tree parent []

        xs ->
            Tree.tree parent <| List.map (buildTree getChildren) xs


{-| Build a `Tree.Forest b` (aka `LList Tree b`) from `Zipper` children
-}
buildForest : (Zipper a -> Tree b) -> Zipper a -> List (Tree b)
buildForest mkTree =
    Zipper.children >> List.map (Zipper.fromTree >> mkTree)


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
