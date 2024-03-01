module Filter exposing
    ( FilterNode(..)
    , Value(..)
    , parse
    , serialize
    )

import Lisp exposing
    ( Atom(..)
    , Expr(..)
    )

type Value
    = Str String
    | Number Float


type FilterNode
    = Everything
    | TzAware
    | Formula
    | FormulaContents String
    | ByName String
    | BySource String
    | ByCache
    | ByCachePolicy String
    | ByMetakey String
    | ByMetaITem String Value
    | ByInternalMetaitem String Value
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
                        "by.everything" -> Ok Everything
                        "by.tzaware" -> Ok TzAware
                        "by.formula" -> Ok Formula
                        "by.name" -> onestring "name" args ByName
                        "by.source" -> onestring "source" args BySource
                        "by.metakey" -> onestring "metakey" args ByMetakey
                        "by.formulacontents" -> onestring "formulacontents" args FormulaContents
                        "by.metaitem" -> twoargs "metaitem" args ByMetaITem
                        "by.internalmetaitem" -> twoargs "inetrnalmetaitem" args ByInternalMetaitem
                        "by.cache" -> Ok ByCache
                        "by.cachepolicy" -> onestring "cachepolicy" args ByCachePolicy
                        "=" -> twoargs "=" args Eq
                        "<" -> twoargs "<" args Lt
                        "<=" -> twoargs "<=" args Lte
                        ">" -> twoargs ">" args Gt
                        ">=" -> twoargs ">=" args Gte
                        _ -> Err "bad operator "
                _ -> Err "bad operator "


serialize node =
    case node of
        Everything ->
            Expression [ Atom <| Symbol "by.everything" ]

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

        BySource source ->
            Expression [ Atom <| Symbol "by.source"
                       , Atom <| String source
                       ]

        ByCache ->
            Expression [ Atom <| Symbol "by.cache" ]

        ByCachePolicy policy ->
            Expression [ Atom <| Symbol "by.cachepolicy"
                       , Atom <| String policy
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

        ByInternalMetaitem key val ->
            case val of
                Str str ->
                    Expression [ Atom <| Symbol "by.internalmetaitem"
                               , Atom <| String key
                               , Atom <| String str
                               ]
                Number num ->
                    Expression [ Atom <| Symbol "by.internalmetaitem"
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
