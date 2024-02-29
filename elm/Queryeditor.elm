module Queryeditor exposing
    ( FilterNode(..)
    , Value(..)
    , main
    , parse
    , serialize
    )

import Browser
import Html as H
import Lisp exposing
    ( Atom(..)
    , Expr(..)
    )


type alias Model =
    { baseurl : String }


type Value
    = Str String
    | Number Float


type FilterNode
    = TzAware
    | Formula
    | FormulaContents String
    | ByName String
    | ByMetakey String
    | ByMetaITem String Value
    | Eq String Value
    | Lt String Value
    | Gt String Value
    | Lte String Value
    | Gte String Value
    | Not FilterNode
    | And (List FilterNode)
    | Or (List FilterNode)


signature : List Atom -> Result String (Atom, List Atom)
signature expr =
    case expr of
        [ op ] -> Ok (op, [])
        (op::args) -> Ok (op, args)
        _ -> Err "missing arguments"


onestring opname args op =
    case args of
        [ name ] ->
            case name of
                String str -> Ok <| op str
                _ -> Err <| "bad arguments for " ++ opname
        _ -> Err <| "bad arguments for " ++ opname


twoargs opname args op =
    case args of
        (arg1::arg2::_) ->
            case arg1 of
                String a1str ->
                    case arg2 of
                        String a2str -> Ok <| op a1str (Str a2str)
                        Int a2int -> Ok <| op a1str (Number <| toFloat a2int)
                        Float a2float -> Ok <| op a1str (Number a2float)
                        _ -> Err <| "bad arguments for " ++ opname
                _ -> Err <| "bad arguments for " ++ opname
        _ -> Err <| "bad arguments for " ++ opname


parse expr =
    case signature(expr) of
        Err err -> Err err
        Ok (op, args) ->
            case op of
                Symbol opname ->
                    case opname of
                        "by.tzaware" -> Ok TzAware
                        "by.formula" -> Ok Formula
                        "by.name" -> onestring "name" args ByName
                        "by.metakey" -> onestring "metakey" args ByMetakey
                        "by.formulacontents" -> onestring "formulacontents" args FormulaContents
                        "by.metaitem" -> twoargs "metaitem" args ByMetaITem
                        "=" -> twoargs "=" args Eq
                        "<" -> twoargs "<" args Lt
                        "<=" -> twoargs "<=" args Lte
                        ">" -> twoargs ">" args Gt
                        ">=" -> twoargs ">=" args Gte
                        _ -> Err "bad operator "
                _ -> Err "bad operator "


serialize node =
    case node of
        TzAware ->
            Expression [ Atom <| Symbol "by.tzaware" ]

        Formula ->
            Expression [ Atom <| Symbol "by.formula" ]

        FormulaContents contents ->
            Expression [ Atom <| Symbol "by.formula"
                       , Atom <| String contents
                       ]

        ByName name ->
            Expression [ Atom <| Symbol "by.name"
                       , Atom <| String name
                       ]

        ByMetakey key ->
            Expression [ Atom <| Symbol "by.metakey"
                       , Atom <| String key
                       ]

        ByMetaITem key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol "by.metaitem"
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol "by.metaitem"
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Eq key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol "="
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol "="
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Gt key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol ">"
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol ">"
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Gte key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol ">="
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol ">="
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Lt key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol "<"
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol "<"
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Lte key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol "<="
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol "<="
                               , Atom <| String key
                               , Atom <| Float num
                               ]

        Not expr ->
            Expression [ Atom <| Symbol "by.not"
                       , serialize expr
                       ]

        And expr ->
            Expression [ Atom <| Symbol "by.and"
                       , Expression <| List.map serialize expr
                       ]

        Or expr ->
            Expression [ Atom <| Symbol "by.or"
                       , Expression <| List.map serialize expr
                       ]


type Msg =
    Noop


view model =
    H.div [] []


update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


type alias Input =
    { baseurl : String }


main : Program Input Model Msg
main =
       let
           init input =
               let
                   model =
                       { baseurl = input.baseurl }
               in
               ( model, Cmd.none )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }

