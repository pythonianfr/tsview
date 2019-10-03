module TsView.Formula.Spec exposing
    ( EditionNode
    , Input
    , ListAction(..)
    , Model
    , Msg(..)
    , Spec
    , SpecType(..)
    , Value(..)
    , buildEditionNode
    , buildEditionTree
    , buildForest
    , buildSpecTree
    , emptyInput
    , fromString
    , getFirstChild
    , getSpecType
    , getUnionCurrentType
    , isOptArg
    , listOperators
    , readInput
    , spec
    , toString
    , valueToString
    )

import Either exposing (Either(..))
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)


type SpecType
    = Int
    | Float
    | String
    | Date
    | SearchString
    | Series
    | SList SpecType
    | Union (Nonempty SpecType)
    | NamedArg String SpecType
    | OptArgs (List SpecType)
    | Operator String (List SpecType) SpecType


type alias Spec =
    Nonempty SpecType


type alias EditionFlags =
    { isOpen : Bool
    , isRemovable : Bool
    }


type Value
    = IntValue ( String, Int )
    | FloatValue ( String, Float )
    | StringValue String
    | DateValue String


type alias Input =
    Either ( String, String ) (Maybe Value)


emptyInput : Input
emptyInput =
    Right Nothing


type alias EditionNode =
    { editFlags : EditionFlags
    , specType : SpecType
    , input : Input
    }


type alias Model =
    { spec : Spec
    , tree : Tree EditionNode
    }


type ListAction
    = ListAdd
    | ListRemove


type Msg
    = ToggleNode (Zipper EditionNode)
    | EditList (Zipper EditionNode) ListAction
    | EditNode (Zipper EditionNode) String


toString : SpecType -> String
toString x =
    case x of
        Int ->
            "int"

        Float ->
            "float"

        String ->
            "string"

        Date ->
            "date"

        Series ->
            "series"

        _ ->
            "series"


fromString : String -> SpecType
fromString x =
    case x of
        "int" ->
            Int

        "float" ->
            Float

        "string" ->
            String

        "date" ->
            Date

        "series" ->
            Series

        _ ->
            Series


valueToString : Value -> String
valueToString value =
    case value of
        IntValue ( s, _ ) ->
            s

        FloatValue ( s, _ ) ->
            s

        StringValue s ->
            s

        DateValue s ->
            s


spec : Spec
spec =
    NE.Nonempty
        (Operator
            "series"
            [ NamedArg "name" SearchString ]
            (OptArgs
                [ NamedArg "fill" String
                , NamedArg "prune" String
                ]
            )
        )
        [ Operator
            "*"
            [ NamedArg "a" (Union (NE.Nonempty Int [ Float, Series ]))
            , NamedArg "b" (Union (NE.Nonempty Series [ Float, Int ]))
            ]
            (OptArgs [])
        , Operator
            "+"
            [ NamedArg "a" (Union (NE.Nonempty Int [ Float, Series ]))
            , NamedArg "b" (Union (NE.Nonempty Series [ Float, Int ]))
            ]
            (OptArgs [])
        , Operator
            "add"
            [ NamedArg "s1" Series
            , NamedArg "s2" Series
            ]
            (OptArgs [])
        , Operator
            "div"
            [ NamedArg "s1" Series
            , NamedArg "s2" Series
            ]
            (OptArgs [])
        , Operator
            "mul"
            [ NamedArg "serieslist" (SList Series) ]
            (OptArgs [])
        , Operator
            "clip"
            [ NamedArg "series" Series ]
            (OptArgs
                [ NamedArg "max" Float
                , NamedArg "min" Float
                ]
            )
        , Operator
            "priority"
            [ NamedArg "serieslist" (SList Series) ]
            (OptArgs [])
        , Operator
            "slice"
            [ NamedArg "series" Series ]
            (OptArgs
                [ NamedArg "fromdate" Date
                , NamedArg "todate" Date
                ]
            )
        ]


listOperators : Spec -> List ( String, SpecType )
listOperators =
    let
        f a b =
            case a of
                Operator name _ _ ->
                    ( name, a ) :: b

                _ ->
                    b
    in
    NE.toList >> List.foldr f []


listSpecNodes : SpecType -> List SpecType
listSpecNodes specType =
    case specType of
        Operator _ args kargs ->
            case kargs of
                OptArgs [] ->
                    args

                _ ->
                    args ++ [ kargs ]

        OptArgs xs ->
            xs

        NamedArg _ x ->
            [ x ]

        Union xs ->
            [ NE.head xs ]

        Series ->
            [ NE.head spec ]

        SList x ->
            listSpecNodes x

        x ->
            []


buildSpecTree : SpecType -> Tree SpecType
buildSpecTree =
    Tree.build listSpecNodes


{-| Build a `Tree.Forest b` (aka `LList Tree b`) from `Zipper` children
-}
buildForest : (Zipper a -> Tree b) -> Zipper a -> Tree.Forest b
buildForest mkTree =
    Zipper.openAll >> List.map mkTree >> LL.fromList


buildEditionNode : Zipper SpecType -> Tree EditionNode
buildEditionNode zipper =
    let
        specType =
            Zipper.current zipper

        isOpen =
            case specType of
                OptArgs _ ->
                    False

                _ ->
                    True

        isRemovable =
            case Zipper.up zipper |> Maybe.map Zipper.current of
                Just (SList _) ->
                    True

                _ ->
                    False
    in
    Tree.Tree
        (EditionNode
            (EditionFlags isOpen isRemovable)
            specType
            emptyInput
        )
        (buildForest buildEditionNode zipper)


buildEditionTree : SpecType -> Tree EditionNode
buildEditionTree =
    buildSpecTree >> Zipper.fromTree >> buildEditionNode


getFirstChild : Zipper a -> Maybe (Zipper a)
getFirstChild =
    Zipper.open (always True)


getSpecType : Zipper EditionNode -> SpecType
getSpecType =
    Zipper.current >> .specType


isOptArg : Zipper EditionNode -> Bool
isOptArg =
    let
        f : Zipper EditionNode -> Bool
        f n =
            case Zipper.current n |> .specType of
                OptArgs _ ->
                    True

                _ ->
                    False
    in
    Zipper.up >> Maybe.map f >> Maybe.withDefault False


getUnionCurrentType : SpecType -> Zipper EditionNode -> SpecType
getUnionCurrentType defaultSpecType =
    let
        getUnionChild zipper =
            case getSpecType zipper of
                Union _ ->
                    getFirstChild zipper

                _ ->
                    Nothing
    in
    getUnionChild
        >> Maybe.map getSpecType
        >> Maybe.withDefault defaultSpecType


readInput : Input -> SpecType -> String -> Input
readInput oldInput specType s =
    let
        strInput =
            Right <| Just <| StringValue s

        numInput valueConstructor numConv strType =
            let
                mess =
                    ( s, "Could not convert \"" ++ s ++ "\" as " ++ strType )
            in
            Either.fromMaybe mess <|
                Maybe.map (Just << valueConstructor << Tuple.pair s) <|
                    numConv s
    in
    if s == "" then
        emptyInput

    else
        case specType of
            Int ->
                numInput IntValue String.toInt "Int"

            Float ->
                numInput FloatValue String.toFloat "Float"

            String ->
                strInput

            Date ->
                strInput

            SearchString ->
                strInput

            _ ->
                oldInput
