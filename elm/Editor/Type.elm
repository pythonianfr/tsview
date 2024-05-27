module Editor.Type exposing (..)

import Either exposing (Either)

import List.Nonempty as NE

import AssocList as Assoc

import ParserExtra exposing (ParserErrors)


type alias Key = String

type alias KAssoc a = Assoc.Dict Key a


-- Spec description

type LiteralType
    = Bool
    | Int
    | Number
    | String
    | TimestampString
    | SeriesName


type OperatorOutputType
    = OperatorOutputType String


type PrimitiveType
    = Literal LiteralType
    | OperatorOutput OperatorOutputType


type Packed =
    Packed PrimitiveType


type ArgType
    = PrimitiveType PrimitiveType
    | UnionType (NE.Nonempty PrimitiveType)
    | PackedType Packed


type ReturnType
    = ReturnPrimitiveType PrimitiveType
    | ReturnList PrimitiveType


type LiteralExpr
    = BoolExpr Bool
    | IntExpr Int
    | NumberExpr Float
    | StringExpr String
    | TimestampExpr String


type alias Operator =
    { name : String
    , args : KAssoc ArgType
    , optArgs : KAssoc ( ArgType, Maybe LiteralExpr )
    , return : ReturnType
    }


type alias Operators = KAssoc Operator

type alias Spec = Assoc.Dict ReturnType Operators

type alias SpecErrors = Maybe (NE.Nonempty String)

type alias SpecConfig = { reduce : Bool }

type alias ReturnTypeStr = String


-- TypedExpr description

type PrimitiveExpr
    = LiteralExpr LiteralType (Maybe LiteralExpr) -- Maybe handles nil
    | OperatorExpr TypedOperator


type ArgExpr
    = PrimitiveExpr PrimitiveExpr
    | UnionExpr (NE.Nonempty PrimitiveType) ( PrimitiveType, PrimitiveExpr )
    | VarArgsExpr Packed (List PrimitiveExpr)
    | PackedExpr Packed TypedOperator


type alias ArgExprs = KAssoc ArgExpr


type alias TypedOperator =
    { operator : Operator
    , typedArgs : ArgExprs
    , typedOptArgs : ArgExprs
    }


-- Formula description

type alias FormulaCode = String


type alias CurrentFormula =
    Either (FormulaCode, ParserErrors) (FormulaCode, TypedOperator)


-- utils

returnTypeFromString : String -> ReturnType
returnTypeFromString s =
    OperatorOutputType s |> OperatorOutput |> ReturnPrimitiveType

makeEmptyOperator : ReturnTypeStr -> Operator
makeEmptyOperator s =
    returnTypeFromString s |> Operator "" Assoc.empty Assoc.empty

voidOperator : Operator
voidOperator = makeEmptyOperator "void"

makeEmptyTypedOperator : ReturnTypeStr -> TypedOperator
makeEmptyTypedOperator s =
    TypedOperator (makeEmptyOperator s) Assoc.empty Assoc.empty

getCode : CurrentFormula -> FormulaCode
getCode currentFormula =
    Either.unpack Tuple.first Tuple.first currentFormula
