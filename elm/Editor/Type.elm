module Editor.Type exposing (..)

import Either exposing (Either)
import Maybe.Extra as Maybe

import AssocList as Assoc
import List.Nonempty as NE
import List.Nonempty.Ancillary as NEA


type alias Key = String

type alias KAssoc a = Assoc.Dict Key a


-- Spec description

type LiteralType
    = Bool -- checkbox
    | Int  -- standard input
    | Number -- standard input
    | String -- standard input
    | TimestampString -- datetime picker
    | SearchString -- catalog browser


type OperatorOutputType = OperatorOutputType String


type PrimitiveType
    = Literal LiteralType
    | OperatorOutput OperatorOutputType
    | Union (NE.Nonempty PrimitiveType)


type CompositeType
    = VarArgs PrimitiveType
    | Packed PrimitiveType -- special purpose list for varargs


type SpecType
    = PrimitiveType PrimitiveType
    | CompositeType CompositeType


-- Concrete values in the formula

type LiteralExpr
    = BoolExpr Bool
    | IntExpr Int
    | NumberExpr Float
    | StringExpr String
    | TimestampExpr String


type alias Operator =
    { name : String
    , args : KAssoc SpecType
    , optArgs : KAssoc ( SpecType, Maybe LiteralExpr )
    , return : SpecType
    }


type alias Spec = KAssoc Operator

type alias SpecErrors = Maybe (NE.Nonempty String)


-- TypedExpr description

type BaseReturnType
    = ReturnLiteral LiteralType
    | ReturnOperatorOutput OperatorOutputType


type ReturnType
    = BaseReturnType BaseReturnType
    | ReturnPacked (NE.Nonempty BaseReturnType)


type alias TypedOperator =
    { operator : Operator
    , typedArgs : KAssoc TypedExpr
    , typedOptArgs : KAssoc TypedExpr
    , returnType : ReturnType
    }


type PrimitiveExpr
    = LiteralExpr LiteralType (Maybe LiteralExpr) -- Maybe handles nil
    | UnionExpr (NE.Nonempty PrimitiveType) ( PrimitiveType, PrimitiveExpr )
    | OperatorExpr TypedOperator


type CompositeExpr
    = VarArgsExpr PrimitiveType (List PrimitiveExpr)
    | PackedExpr PrimitiveType TypedOperator


type TypedExpr
    = PrimitiveExpr PrimitiveExpr
    | CompositeExpr CompositeExpr


type alias TypedExprs = KAssoc TypedExpr


-- Formula description

type alias FormulaCode = String


type alias Annotation =
    { rowPos : Int
    , colPos : Int
    , errMess : String
    }


type alias ParserError =
    { annotation : Annotation
    , contextStack : Maybe (NE.Nonempty Annotation)
    }


type alias ParserErrors = NE.Nonempty ParserError


type alias CurrentFormula =
    Either (FormulaCode, ParserErrors) (FormulaCode, TypedOperator)


-- PrimitiveType utils

findBaseReturnType : PrimitiveType -> BaseReturnType
findBaseReturnType p = case p of
    Literal t -> ReturnLiteral t

    OperatorOutput t -> ReturnOperatorOutput t

    Union xs -> findBaseReturnType <| NE.head xs

listBaseReturnType : PrimitiveType -> NE.Nonempty BaseReturnType
listBaseReturnType primitiveType = case primitiveType of
    Literal t ->
        NE.singleton <| ReturnLiteral t

    OperatorOutput t ->
        NE.singleton <| ReturnOperatorOutput t

    Union xs ->
        NE.concatMap listBaseReturnType xs


-- Operators listed by BaseReturnType or Packed[BaseReturnType]

type alias Operators = KAssoc Operator

type alias GOperators =
    Assoc.Dict BaseReturnType Operators -- should be real Dict


type alias GSpec =
    { spec : Spec
    , gOperators : GOperators
    }


fromSpecType : SpecType -> NE.Nonempty BaseReturnType
fromSpecType specType = case specType of
    PrimitiveType t ->
        listBaseReturnType t

    CompositeType (VarArgs t) -> -- should not exist in practice
        listBaseReturnType t

    CompositeType (Packed t) ->
        listBaseReturnType t

fromReturnType : ReturnType -> NE.Nonempty BaseReturnType
fromReturnType returnType = case returnType of
    BaseReturnType t -> NE.singleton t

    ReturnPacked ts -> ts

buildGSpec : Spec -> GSpec
buildGSpec spec =
    let
        pairingOperator : Operator -> List (BaseReturnType, Operator)
        pairingOperator op = List.map
            (\gType -> (gType, op))
            (fromSpecType op.return |> NE.toList)

        addOperator : (BaseReturnType, Operator) -> GOperators -> GOperators
        addOperator (gType, {name} as op) = Assoc.update
            (gType)
            (Maybe.unwrap (Assoc.singleton name op) (Assoc.insert name op)
                >> Just)

    in Assoc.values spec
        |> List.concatMap pairingOperator
        |> List.foldl addOperator Assoc.empty
        |> GSpec spec

findOperators : GSpec -> ReturnType -> Either String Operators
findOperators {gOperators} returnType = 
    NEA.traverse
        (\gType -> Assoc.get gType gOperators)
        (fromReturnType returnType)
        |> Maybe.map (NE.foldl1 Assoc.union)
        |> Either.fromMaybe
            ("No operator for type " ++ renderReturnType returnType)

findOperator : GSpec -> ReturnType -> Key -> Either String Operator
findOperator gSpec returnType key =
    findOperators gSpec returnType |> Either.andThen
        (Assoc.get key >> Either.fromMaybe ("No operator for " ++ key))


-- basic rendering
renderLiteralType : LiteralType -> String
renderLiteralType t = case t of
    Int -> "Int"

    Number -> "Number"

    String -> "String"

    Bool -> "Bool"

    TimestampString -> "Timestamp"

    SearchString -> "SearchString"

renderOperatorOutputType : OperatorOutputType -> String
renderOperatorOutputType (OperatorOutputType s) = s

renderBaseReturnType : BaseReturnType -> String
renderBaseReturnType baseReturnType = case baseReturnType of
    ReturnLiteral x -> renderLiteralType x

    ReturnOperatorOutput x -> renderOperatorOutputType x 

renderReturnType : ReturnType -> String
renderReturnType returnType = case returnType of
    BaseReturnType x -> renderBaseReturnType x

    ReturnPacked xs ->
        let
            packedTypes = NE.map renderBaseReturnType xs
                |> NE.toList
                |> String.join ", "

        in "Packed[" ++ packedTypes ++ "]"


-- utils
voidOperator : Operator
voidOperator =
    OperatorOutputType "void"
        |> OperatorOutput
        |> PrimitiveType
        |> Operator "" Assoc.empty Assoc.empty

voidTypedOperator : TypedOperator
voidTypedOperator =
    OperatorOutputType "void"
        |> ReturnOperatorOutput
        |> BaseReturnType
        |> TypedOperator voidOperator Assoc.empty Assoc.empty

returnSeries : ReturnType
returnSeries =
    OperatorOutputType "Series"
        |> ReturnOperatorOutput
        |> BaseReturnType
