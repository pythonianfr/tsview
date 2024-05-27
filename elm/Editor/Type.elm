module Editor.Type exposing (..)

import AssocList as Assoc
import List.Nonempty as NE exposing (Nonempty)


type alias Key =
    String


type alias KAssoc a =
    Assoc.Dict Key a


-- primitive types *for the edition tree* literal inputs handling
-- no Nil is wanted there (as it is only used to express the absence
-- of a keyword value and is represented by a *blank* field handled
-- with a Maybe)
type LiteralType
    = Bool -- checkbox
    | Int  -- standard input
    | Number -- standard input
    | String -- standard input
    | TimestampString -- datetime picker
    | SearchString -- catalog browser
    | Void -- no input


-- primitive types used in the spec
type SpecType
    = Editable LiteralType
    | Timestamp
    | Series
    | Query
    | Union (Nonempty SpecType)
    -- | Record (Nonempty (String, LiteralType, Maybe EditableValue))
    -- varargs handling
    | Varargs SpecType -- for literal inputs to varargs
    | Packed SpecType -- special purpose list for varargs


-- representation of concrete values in the formula or as written by
-- the user
type EditableValue
    = Nil
    | BoolValue Bool
    | IntValue Int
    | NumberValue Float
    | StringValue String
    | TimestampValue String -- in the future we want to validate timestamps


type alias Operator =
    { name : String
    , args : KAssoc SpecType
    , optArgs : KAssoc ( SpecType, EditableValue ) -- Value is for the default value
    , return : SpecType
    }


type alias Spec =
    KAssoc Operator


-- typed formula for the ui consumption
type TypedExpr
    = TLiteral LiteralType ( String, EditableValue )
    | TUnion (Nonempty SpecType) ( SpecType, TypedExpr )
    | TVarargs SpecType (List TypedExpr)
    | TOperator Operator (KAssoc TypedExpr) (KAssoc TypedExpr)
  
-- utils for no input
voidValue : EditableValue
voidValue = StringValue ""

voidExpr : TypedExpr
voidExpr = TLiteral Void ("", voidValue)
