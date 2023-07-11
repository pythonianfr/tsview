module Lisp exposing
    ( Atom(..)
    , Expr(..)
    , deadendstostr
    , depth
    , lispparser
    , parse
    , quote
    , serialize
    , view
    , width
    )

import Char
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Parser exposing
    ( (|.)
    , (|=)
    , DeadEnd
    , Parser
    , Problem(..)
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


quote = String.fromChar '"'


-- atom parsers

symbolparser : Parser String
symbolparser =
    let
        special_chars = String.toList "_-+*/.<>="
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
        |. Parser.spaces
        |= Parser.oneOf
           [ Parser.map Atom atomparser
           , Parser.lazy (\_ -> lispparser)
           ]
        |. Parser.spaces


lispparser =
    parens <|
        Parser.succeed Expression
            |= many argsparser


parse formula =
    case Parser.run lispparser formula of
        Ok parsed -> Just parsed
        Err _ -> Nothing


-- helpers

depth formula =
    case formula of
        Atom atom ->
            0
        Expression expr ->
            Maybe.withDefault 0 <|
                List.maximum <| List.map (\e -> (depth e) + 1) expr


width formula =
    case formula of
        Atom atom ->
            case atom of
                Symbol sym ->
                    String.length sym
                Keyword kw ->
                    (String.length kw) + 2
                String str ->
                    String.length str + 2
                Float flo ->
                    String.length <| String.fromFloat flo
                Int int ->
                    String.length <| String.fromInt int
                Bool bo ->
                    2
                Nil ->
                    3

        Expression expr ->
            1 + (List.length expr) + (List.sum <| List.map width expr)


-- serializer

serialize lisp =
    case lisp of
        Atom atom ->
            case atom of
                Symbol sym ->
                    sym
                Keyword kw ->
                    "#:" ++ kw
                String str ->
                    quote ++ str ++ quote
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

view formula overrides =
    case formula of
        Expression expr ->
            [ H.div
                  [ HA.class "highlight" ]
                  [ H.pre [] ([ H.span [] [] ] ++ viewexpr overrides 0 expr)
                  ]
            ]

        _ ->
            [ H.div [] [ H.text "This is not a formula." ] ]


noop = "no-such-operator"


spaces qty =
    String.repeat (4 * qty) " "


viewexpr overrides indent exprs =
    let
        operator =
            case Maybe.withDefault (Atom <| Symbol noop) <| List.head exprs of
                Expression _ -> noop
                Atom atom ->
                    case atom of
                        Symbol sym -> sym
                        _ -> noop

        argslist =
            case List.tail exprs of
                Nothing -> []
                Just rest -> rest

        hasargs =
            (List.length argslist) > 0

        exprswidth =
            List.sum <| List.map width exprs

        breakargs =
            exprswidth > 70

        newindent =
            indent + (if breakargs then 1 else 0)

    in
    List.concat
        [ [ H.span [ HA.class "p" ] [ H.text "(" ] ]
        , [ H.span
                [ HA.class "nv" ]
                [ H.text <| operator ]
          ]
        , List.concat <| viewargs overrides operator newindent breakargs argslist
        , [ H.span [ HA.class "p" ] [ H.text ")" ] ]
        ]


zip = List.map2 Tuple.pair


viewargs overrides operator indent break argslist  =
    let
        override =
            Dict.get operator overrides

        iskeyword arg =
            case arg of
                Atom (Keyword _) -> True
                _ -> False

        keywordargs =
            -- a list describing the keywordiness of the items
            -- shifted because we want to apply keywordiness
            -- to the keyword value (atom following the keyword)
            False :: List.map iskeyword argslist

        wrapindent arg iskw html =
            -- the tricky business of knowning when to break ...
            let
                dobreak = break && not iskw
            in
            List.concat
                [ if dobreak
                  -- a line break, because the pygments css does not work for us there
                  then [ H.br [] [] ]
                  else []
                , if dobreak
                  -- the proper amount of left identation *or* just a single spacer
                  then [ H.span [ HA.class "w" ] [ H.text <| spaces indent ] ]
                  else [ H.span [ HA.class "w" ] [ H.text " " ] ]
                , html
                ]

        wrappedviewatom index arg =
            -- directly call viewatom or call the override plus
            -- viewatom as a fallback
            case override of
                Nothing ->
                    viewatom overrides indent arg
                Just func ->
                    func index arg (viewatom overrides indent) -- partial: eats `arg`
    in
    List.map
        (\(index, (arg, iskw)) -> (wrapindent arg iskw <| wrappedviewatom index arg))
        <| zip
            (List.range 0 (List.length argslist))
            (zip argslist keywordargs)


viewatom overrides indent atomorexpr =
    case atomorexpr of
        Expression expr ->
            viewexpr overrides indent expr

        Atom atom ->
            case atom of
                Symbol sym ->
                    [ H.span [ HA.class "nv" ] [ H.text sym ] ]
                Keyword kw ->
                    [ H.span [ HA.class "ss" ] [ H.text <| "#:" ++ kw ] ]
                String str ->
                    [ H.span [ HA.class "s" ] [ H.text <| quote ++ str ++ quote ] ]
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
