module TsView.Formula.Spec.Type exposing
    ( BaseType(..)
    , ExpType(..)
    , InputType(..)
    , Operator
    , Spec
    , Value(..)
    , getOperators
    , noSpec
    , specToList
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
    , args : List ExpType
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
