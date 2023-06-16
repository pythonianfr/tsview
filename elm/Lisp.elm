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


type Expr
    = Atom Atom
    | Expression (List Expr)


varnameparser : Parser String
varnameparser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


atomparser =
    oneOf [ Parser.map Symbol varnameparser ]


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
