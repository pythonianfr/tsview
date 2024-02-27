module Queryeditor exposing
    ( FilterNode(..)
    , Value(..)
    , main
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
    | Eq String Value
    | Lt String Value
    | Gt String Value
    | Lte String Value
    | Gte String Value
    | Not FilterNode
    | And (List FilterNode)
    | Or (List FilterNode)


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

