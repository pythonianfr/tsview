module Lisp exposing (parser, Atom(..))

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
    | String String
    | Float Float
    | Int Int
    | Bool Bool
    | Nil


type Expr
    = Atom Atom
    | Expression (List Expr)


-- atom parsers

varnameparser : Parser String
varnameparser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-' || c == '.'
        , reserved = Set.empty
        }


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
        , Parser.map Symbol varnameparser
        , Parser.map String stringparser
        -- intparser will start parsing floats and fail, hence we
        -- need to be able to backtrack from it
        , Parser.backtrackable <|
            Parser.map Int intparser
        , Parser.map Float floatparser
        ]


argsparser =
    many atomparser


exprparser =
    parens <|
        Parser.succeed identity
            |. spaces
            |= argsparser
            |. spaces


parser =
    Parser.succeed identity
        |. spaces
        |= exprparser
        |. spaces
