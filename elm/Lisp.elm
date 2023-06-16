module Lisp exposing (lispparser, Atom(..), Expr(..))

import Char
import Parser exposing
    ( (|.)
    , (|=)
    , Parser
    , spaces
    )
import Parser.Extras exposing (many, parens)
import Set


type Atom
    = Symbol String
    | Keyword String
    | String String
    | Float Float
    | Int Int
    | Bool Bool
    | Nil


type Expr
    = Atom Atom
    | Expression (List Expr)


-- atom parsers

symbolparser : Parser String
symbolparser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-' || c == '.'
        , reserved = Set.empty
        }


keywordparser : Parser String
keywordparser =
    Parser.succeed identity
        |. Parser.symbol "#:"
        |= symbolparser


stringparser : Parser String
stringparser =
    let
        quotechar = '"'
        quote = String.fromChar quotechar
    in
    Parser.succeed identity
        |. Parser.symbol quote
        |= Parser.variable
           { start = always True
           , inner = (/=) quotechar
           , reserved = Set.empty
           }
        |. Parser.symbol quote


floatparser : Parser Float
floatparser =
    Parser.oneOf
        [ Parser.succeed negate |. Parser.symbol "-" |= Parser.float
        , Parser.float
        ]


intparser : Parser Int
intparser =
    Parser.oneOf
        [ Parser.succeed negate |. Parser.symbol "-" |= Parser.int
        , Parser.int
        ]


boolparser : Parser Bool
boolparser =
    Parser.oneOf
        [ Parser.succeed True |. Parser.keyword "#t"
        , Parser.succeed False |. Parser.keyword "#f"
        ]


nilparser : Parser ()
nilparser =
    Parser.keyword "nil"


atomparser : Parser Atom
atomparser =
    Parser.oneOf
        [ Parser.succeed Nil |. Parser.keyword "nil"
        , Parser.map Bool boolparser
        , Parser.map Keyword keywordparser
        , Parser.map Symbol symbolparser
        , Parser.map String stringparser
        -- intparser will start parsing floats and fail, hence we
        -- need to be able to backtrack from it
        , Parser.backtrackable <|
            Parser.map Int intparser
        , Parser.map Float floatparser
        ]


-- top-level recursive expression parser

argsparser =
    Parser.oneOf
        [ Parser.map Atom atomparser
        , Parser.lazy (\_ -> lispparser)
        ]


lispparser =
    parens <|
        Parser.succeed Expression
            |. spaces
            |= many argsparser
            |. spaces
