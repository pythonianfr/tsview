module Lisp exposing
    ( Atom(..)
    , Expr(..)
    , deadendstostr
    , lispparser
    , parse
    , serialize
    , view
    )

import Char
import Html as H
import Html.Attributes as HA
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


parse formula =
    case Parser.run lispparser formula of
        Ok parsed -> Just parsed
        Err _ -> Nothing


-- serializer

cstr = String.fromChar


serialize lisp =
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


-- view

view formula =
    case formula of
        Expression expr ->
            [ H.div
                  [ HA.class "highlight" ]
                  [ H.pre [ ] <| viewexpr 0 expr ]
            ]

        _ ->
            [ H.div [] [ H.text "This is not a formula." ] ]


noop = "no-such-operator"


viewexpr indent expr =
    let
        operator =
            case Maybe.withDefault (Atom <| Symbol noop) <| List.head expr of
                Expression _ -> noop
                Atom atom ->
                    case atom of
                        Symbol sym -> sym
                        _ -> noop
        argslist =
            case List.tail expr of
                Nothing -> []
                Just rest -> rest
        hasargs =
            (List.length argslist) > 0
    in
    List.concat
        [ [ H.span [ HA.class "p" ] [ H.text "(" ] ]
        , [ H.span
                [ HA.class "nv" ]
                [ H.text <| operator ++ (if hasargs then " " else "") ]
          ]
        , List.concat <| viewargs indent argslist
        , [ H.span [ HA.class "p" ] [ H.text ")" ] ]
        ]


viewargs indent argslist =
    List.intersperse
        [ H.span [ HA.class "w" ] [ H.text " " ] ]
        <| List.map (viewatom indent) argslist


viewatom indent atomorexpr =
    case atomorexpr of
        Expression expr ->
            viewexpr indent expr

        Atom atom ->
            case atom of
                Symbol sym ->
                    [ H.span [ HA.class "nv" ] [ H.text sym ] ]
                Keyword kw ->
                    [ H.span [ HA.class "ss" ] [ H.text <| "#:" ++ kw ] ]
                String str ->
                    [ H.span [ HA.class "s" ] [ H.text <| (cstr '"') ++ str ++ (cstr '"') ] ]
                Float flo ->
                    [ H.span [ HA.class "mf" ] [ H.text <| String.fromFloat flo ] ]
                Int int ->
                    [ H.span [ HA.class "mf" ] [ H.text <| String.fromInt int ] ]
                Bool bool ->
                    [ H.span [ HA.class "mv" ] [ H.text <| if bool then "#t" else "#f"  ] ]
                Nil ->
                    [ H.span [ HA.class "mv" ] [ H.text "nil" ] ]


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
