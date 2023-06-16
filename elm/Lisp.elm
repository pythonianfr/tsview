module Lisp exposing (parser, Atom(..))

import Char
import Parser exposing
    ( (|.)
    , (|=)
    , oneOf
    , Parser
    , spaces
    , succeed
    , variable
    )
import Parser.Extras exposing (many, parens)
import Set


type Atom
    = Symbol String
    | String String


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


atomparser =
    oneOf [ Parser.map Symbol varnameparser
          , Parser.map String stringparser
          ]


argsparser =
    many atomparser


exprparser =
    parens <|
        succeed identity
            |. spaces
            |= argsparser
            |. spaces


parser =
    succeed identity
        |. spaces
        |= exprparser
        |. spaces
