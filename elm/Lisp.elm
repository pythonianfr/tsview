module Lisp exposing
    ( Atom(..)
    , Expr(..)
    , deadendstostr
    , lispparser
    , serialize
    )

import Char
import Parser exposing
    ( (|.)
    , (|=)
    , DeadEnd
    , Parser
    , Problem(..)
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
    let
        special_chars = String.toList "_-+*/."
        accept_chars =
            \c -> List.member c special_chars
    in
    Parser.variable
        { start = \c -> Char.isLower c || accept_chars c
        , inner = \c -> Char.isAlphaNum c || accept_chars c
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
    Parser.succeed identity
        |. spaces
        |= Parser.oneOf
           [ Parser.map Atom atomparser
           , Parser.lazy (\_ -> lispparser)
           ]
        |. spaces


lispparser =
    parens <|
        Parser.succeed Expression
            |= many argsparser


-- serializer

serialize lisp =
    let
        cstr = String.fromChar
    in
    case lisp of
        Atom atom ->
            case atom of
                Symbol sym ->
                    sym
                Keyword kw ->
                    "#:" ++ kw
                String str ->
                    (cstr '"') ++ str ++ (cstr '"')
                Float flo ->
                    String.fromFloat flo
                Int int ->
                    String.fromInt int
                Bool bo ->
                    if bo then "#t" else "#f"
                Nil ->
                    "nil"

        Expression expr ->
            "(" ++ (String.join " " <| List.map serialize expr) ++ ")"


-- errors basic decoder

deadendstostr deadends =
    let
        tostring : DeadEnd -> String
        tostring deadend =
            let
                position : String
                position =
                    "row " ++ String.fromInt deadend.row ++ " " ++
                    "col " ++ String.fromInt deadend.col
            in
            case deadend.problem of
                Expecting str ->
                    "Expecting `" ++ str ++ "` at " ++ position

                ExpectingInt ->
                    "Expecting an Int at " ++ position

                ExpectingHex ->
                    "Expecting an Hex at " ++ position

                ExpectingOctal ->
                    "Expecting an Octal at " ++ position

                ExpectingBinary ->
                    "Expecting a Binary at " ++ position

                ExpectingFloat ->
                    "Expecting a Float at " ++ position

                ExpectingNumber ->
                    "Expecting a Number at " ++ position

                ExpectingVariable ->
                    "Expecting a Variable at " ++ position

                ExpectingSymbol str ->
                    "Expecting Symbol `" ++ str ++ "` at " ++ position

                ExpectingKeyword str ->
                    "Expecting Keyword `" ++ str ++ "` at " ++ position

                ExpectingEnd ->
                    "Expecting End at " ++ position

                UnexpectedChar ->
                    "Unexpected Char at " ++ position

                Problem str ->
                    "Problem `" ++ str ++ "` at " ++ position

                BadRepeat ->
                    "Bad repeat at " ++ position
    in
    List.foldl (++) "" (List.map tostring deadends)
