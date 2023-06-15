module Lisp exposing (parser, Atom(..))

import Char
import Parser exposing
    ( (|.)
    , (|=)
    , Parser
    , spaces
    , succeed
    , variable
    )
import Parser.Extras exposing (parens)
import Set


type Atom
    = Symbol String


symbolparser : Parser String
symbolparser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


exprparser =
    parens <|
        succeed Symbol
            |= symbolparser


inlist item =
    [ item ]


parser =
    succeed inlist
        |. spaces
        |= exprparser
        |. spaces
