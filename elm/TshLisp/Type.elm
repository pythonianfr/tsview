module TshLisp.Type exposing (..)

import AssocList as Assoc
import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)


type alias Key =
    String


type alias KAssoc a =
    Assoc.Dict Key a


type InputType
    = Bool
    | Int
    | Number
    | String
    | Timestamp
    | SearchString


type SpecType
    = BaseInput InputType
    | Series
    | List SpecType
    | Union (Nonempty SpecType)


type Value
    = NIL
    | BoolValue Bool
    | IntValue Int
    | NumberValue Float
    | StringValue String
    | TimestampValue String


type alias Operator =
    { name : String
    , args : KAssoc SpecType
    , optArgs : KAssoc ( SpecType, Value )
    , return : SpecType
    }


type alias Spec =
    KAssoc Operator


type SExpr
    = SInput InputType ( String, Value )
    | SSeries SExpr
    | SList SpecType (List SExpr)
    | SUnion (Nonempty SpecType) ( SpecType, SExpr )
    | SOperator Operator (KAssoc SExpr) (KAssoc SExpr)



-- XXX TODO, TshLisp.Parser generic proposal, dropping TsView.Formula.Spec dependency.


parseFormula : Spec -> String -> Either String SExpr
parseFormula spec formulaCode =
    Left "TODO"
