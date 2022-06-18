module TshLisp.Type exposing (..)


type LiteralValue
    = NIL
    | BoolValue Bool
    | NumberValue Float
    | StringValue String


type SExpr
    = SInput LiteralValue
    | SOperator String (List SExpr) (List ( String, SExpr ))
