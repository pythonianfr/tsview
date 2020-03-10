module TsView.Formula.EditionTree.Type exposing
    ( Arg(..)
    , ArgType(..)
    , EditionNode
    , EditionType(..)
    , Model
    , Msg
    , Operator(..)
    , OptArg(..)
    , OptArgs(..)
    , Value(..)
    , buildEditionTree
    , buildInitialTree
    , fromEditionType
    , fromSpecOperator
    , probeArgSelector
    , probeSelector
    , specToList
    )

import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (buildForest, buildTree)


type ArgType
    = ArgType S.ExpType


type Arg
    = Arg ArgType


type OptArg
    = OptArg String ArgType


type OptArgs
    = OptArgs (Nonempty OptArg)
    | NoOptArgs


type Operator
    = Operator String (List Arg) OptArgs


fromSpecOperator : S.Operator -> Operator
fromSpecOperator op =
    let
        optArgs =
            List.map (\( k, x ) -> OptArg k (ArgType x)) op.kargs
                |> NE.fromList
                |> Maybe.map OptArgs
                |> Maybe.withDefault NoOptArgs
    in
    Operator
        op.name
        (List.map (ArgType >> Arg) op.args)
        optArgs


type EditionType
    = ArgTypeT ArgType
    | SelectorT S.BaseType
    | InputSelectorT S.InputType
    | ArgT Arg
    | OptArgT OptArg
    | OptArgsT OptArgs
    | OperatorT Operator
    | ReturnTypeT (Nonempty S.BaseType)


noEditionType : EditionType
noEditionType =
    ArgTypeT <| ArgType <| S.ExpBaseType <| S.BaseInput <| S.Bool


type alias EditionFlags =
    { isOpen : Bool
    , isRemovable : Bool
    }


type Value
    = Empty
    | BoolValue Bool
    | IntValue Int
    | NumberValue Float
    | StringValue String
    | TimestampValue String


type alias Input =
    ( String, Either String Value )


emptyInput : Input
emptyInput =
    ( "", Right Empty )


type alias EditionNode =
    { editionFlags : EditionFlags
    , editionType : EditionType
    , input : Input
    }


fromEditionType : EditionType -> EditionNode
fromEditionType editionType =
    EditionNode (EditionFlags True False) editionType emptyInput


type alias Msg =
    ()


type alias Model =
    { spec : S.Spec
    , errors : Maybe (Nonempty String)
    }


probeSelector : S.Spec -> S.BaseType -> EditionType
probeSelector spec baseType =
    case baseType of
        S.Series ->
            SelectorT S.Series

        S.BaseInput x ->
            case S.getOperators baseType spec of
                Just _ ->
                    InputSelectorT x

                Nothing ->
                    ArgTypeT <| ArgType <| S.ExpBaseType <| baseType


probeArgSelector : S.Spec -> ArgType -> EditionType
probeArgSelector spec ((ArgType expType) as argType) =
    case expType of
        S.ExpBaseType x ->
            probeSelector spec x

        _ ->
            ArgTypeT argType


fromExpType : S.Spec -> S.ExpType -> List EditionType
fromExpType spec expType =
    let
        toArg =
            ArgType >> probeArgSelector spec >> List.singleton
    in
    case expType of
        S.Union xs ->
            toArg (NE.head xs)

        S.SList x ->
            toArg x

        S.ExpBaseType x ->
            []


listTypes : S.Spec -> EditionType -> List EditionType
listTypes spec editionType =
    -- takes a type and returns its constituents
    -- to help build subnodes for the whole
    -- edition tree node
    case editionType of
        ReturnTypeT xs ->
            [ SelectorT (NE.head xs) ]

        SelectorT baseType ->
            S.getOperators baseType spec
                |> Maybe.map (NE.head >> fromSpecOperator >> OperatorT)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        InputSelectorT inputType ->
            [ S.BaseInput inputType |> S.ExpBaseType |> ArgType |> ArgTypeT ]

        OperatorT (Operator _ args optArgs) ->
            List.map ArgT args ++ [ OptArgsT optArgs ]

        OptArgsT NoOptArgs ->
            []

        OptArgsT (OptArgs xs) ->
            NE.map OptArgT xs |> NE.toList

        ArgT (Arg x) ->
            [ probeArgSelector spec x ]

        OptArgT (OptArg _ x) ->
            [ probeArgSelector spec x ]

        ArgTypeT (ArgType x) ->
            fromExpType spec x


buildTypeTree : S.Spec -> EditionType -> Tree EditionType
buildTypeTree spec editionType =
    buildTree (listTypes spec) editionType


buildEditionNode : Zipper EditionType -> Tree EditionNode
buildEditionNode zipper =
    let
        editionType =
            Zipper.label zipper

        isOpen =
            case editionType of
                OptArgsT _ ->
                    False

                _ ->
                    True

        isRemovable =
            case Zipper.parent zipper |> Maybe.map Zipper.label of
                Just (ArgTypeT (ArgType (S.SList _))) ->
                    True

                _ ->
                    False
    in
    Tree.tree
        (EditionNode
            (EditionFlags isOpen isRemovable)
            editionType
            emptyInput
        )
        (buildForest buildEditionNode zipper)


buildEditionTree : S.Spec -> EditionType -> Tree EditionNode
buildEditionTree spec editionType =
    buildTypeTree spec editionType |> Zipper.fromTree |> buildEditionNode


buildInitialTree : S.Spec -> Tree EditionNode
buildInitialTree spec =
    S.specToList spec
        |> List.map Tuple.first
        |> NE.fromList
        |> Maybe.map ReturnTypeT
        |> Maybe.withDefault noEditionType
        |> buildEditionTree spec


specToList : S.Spec -> List ( S.BaseType, Nonempty EditionType )
specToList =
    S.specToList
        >> List.map (Tuple.mapSecond (NE.map (fromSpecOperator >> OperatorT)))
