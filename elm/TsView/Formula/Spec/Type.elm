module TsView.Formula.Spec.Type exposing
    ( BaseType(..)
    , ExpType(..)
    , InputType(..)
    , Operator
    , Spec
    , Value(..)
    , getOperator
    , getOperators
    , matchBaseType
    , matchExpType
    , noSpec
    , specToList
    , strBaseType
    , strExpType
    , strInputType
    )

import AssocList as Assoc
import List.Nonempty as NE exposing (Nonempty)


type InputType
    = Int
    | Number
    | String
    | Bool
    | Timestamp
    | SearchString


type BaseType
    = BaseInput InputType
    | Series


type ExpType
    = ExpBaseType BaseType
    | SList ExpType
    | Union (Nonempty ExpType)


type Value
    = Empty
    | BoolValue Bool
    | IntValue Int
    | NumberValue Float
    | StringValue String
    | TimestampValue String


type alias Operator =
    { name : String
    , args : List (String, ExpType)
    , kargs : List ( String, ExpType, Value )
    , return : ExpType
    }


type alias Spec =
    Assoc.Dict BaseType (Nonempty Operator)


noSpec : Spec
noSpec =
    Assoc.empty


specToList : Spec -> List ( BaseType, Nonempty Operator )
specToList =
    Assoc.toList


getOperators : BaseType -> Spec -> Maybe (Nonempty Operator)
getOperators =
    Assoc.get


getOperator : BaseType -> String -> Spec -> Maybe Operator
getOperator baseType operatorName spec =
    let
        mkOperators =
            NE.toList >> List.map (\op -> ( op.name, op )) >> Assoc.fromList
    in
    getOperators baseType spec
        |> Maybe.andThen (mkOperators >> Assoc.get operatorName)


strInputType : InputType -> String
strInputType iType =
    case iType of
        Int ->
            "Int"

        Number ->
            "Number"

        String ->
            "String"

        Bool ->
            "Bool"

        Timestamp ->
            "Timestamp"

        SearchString ->
            "SearchString"


strBaseType : BaseType -> String
strBaseType bType =
    case bType of
        BaseInput x ->
            strInputType x

        Series ->
            "Series"


strExpType : ExpType -> String
strExpType eType =
    case eType of
        ExpBaseType x ->
            strBaseType x

        SList x ->
            "List[" ++ strExpType x ++ "]"

        Union xs ->
            let
                typesToStr =
                    NE.map strExpType >> NE.toList >> String.join ", "
            in
            "Union[" ++ typesToStr xs ++ "]"


matchType : (a -> String) -> Nonempty a -> String -> Maybe a
matchType toStr xs s =
    NE.toList xs
        |> List.map (\x -> ( toStr x, x ))
        |> Assoc.fromList
        |> Assoc.get s


matchBaseType : Nonempty BaseType -> String -> Maybe BaseType
matchBaseType =
    matchType strBaseType


matchExpType : Nonempty ExpType -> String -> Maybe ExpType
matchExpType =
    matchType strExpType
