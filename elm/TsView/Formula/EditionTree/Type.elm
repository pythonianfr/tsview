module TsView.Formula.EditionTree.Type exposing
    ( Arg(..)
    , EditAction(..)
    , EditionNode
    , EditionTree
    , EditionType(..)
    , Forest
    , Input
    , Model
    , Msg(..)
    , Operator(..)
    , OptArg(..)
    , OptArgs(..)
    , buildEditionTree
    , buildInitialTree
    , emptyInput
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


type alias Forest a =
    List (Tree a)


type Arg
    = Arg String S.ExpType


type OptArg
    = OptArg String S.ExpType S.Value


type OptArgs
    = OptArgs (Nonempty OptArg)
    | NoOptArgs


type Operator
    = Operator String (List Arg) OptArgs


fromSpecOperator : S.Operator -> Operator
fromSpecOperator op =
    let
        optArgs =
            List.map (\( k, x, v ) -> OptArg k x v) op.kargs
                |> NE.fromList
                |> Maybe.map OptArgs
                |> Maybe.withDefault NoOptArgs
    in
    Operator
        op.name
        (List.map (\( k, v) -> Arg k v) op.args)
        optArgs


type EditionType
    = ExpTypeT S.ExpType
    | SelectorT S.BaseType
    | InputSelectorT S.InputType
    | ArgT Arg
    | OptArgT OptArg
    | OptArgsT OptArgs
    | OperatorT Operator
    | ReturnTypeT (Nonempty S.BaseType)


noEditionType : EditionType
noEditionType =
    ExpTypeT <| S.ExpBaseType <| S.BaseInput <| S.Bool


type alias EditionFlags =
    { isOpen : Bool
    }


type alias Input =
    ( String, Either String S.Value )


emptyInput : Input
emptyInput =
    ( "", Right S.Empty )


type alias EditionNode =
    { editionFlags : EditionFlags
    , editionType : EditionType
    , input : Input
    }


type alias EditionTree =
    Tree EditionNode


fromEditionType : EditionType -> EditionNode
fromEditionType editionType =
    EditionNode (EditionFlags True) editionType emptyInput


type EditAction
    = ReadInput String
    | ListAdd
    | ListRemove


type Msg
    = Edit EditionTree
    | RenderFormula EditionTree
    | ToggleNode (Zipper EditionNode)
    | EditNode (Zipper EditionNode) EditAction


type alias Model =
    { spec : S.Spec
    , tree : EditionTree
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
                    ExpTypeT <| S.ExpBaseType <| baseType


probeArgSelector : S.Spec -> S.ExpType -> EditionType
probeArgSelector spec expType =
    case expType of
        S.ExpBaseType x ->
            probeSelector spec x

        _ ->
            ExpTypeT expType


fromExpType : S.Spec -> S.ExpType -> List EditionType
fromExpType spec expType =
    let
        toArg =
            probeArgSelector spec >> List.singleton
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
            [ NE.head xs |> probeSelector spec ]

        SelectorT baseType ->
            S.getOperators baseType spec
                |> Maybe.map (NE.head >> fromSpecOperator >> OperatorT)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        InputSelectorT inputType ->
            [ S.BaseInput inputType |> S.ExpBaseType |> ExpTypeT ]

        OperatorT (Operator _ args NoOptArgs) ->
            List.map ArgT args

        OperatorT (Operator _ args optArgs) ->
            List.map ArgT args ++ [ OptArgsT optArgs ]

        OptArgsT NoOptArgs ->
            []

        OptArgsT (OptArgs xs) ->
            NE.map OptArgT xs |> NE.toList

        ArgT (Arg _ x) ->
            [ probeArgSelector spec x ]

        OptArgT (OptArg _ x _) ->
            [ probeArgSelector spec x ]

        ExpTypeT x ->
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
    in
    Tree.tree
        (EditionNode
            (EditionFlags isOpen)
            editionType
            emptyInput
        )
        (buildForest buildEditionNode zipper)


buildEditionTree : S.Spec -> EditionType -> EditionTree
buildEditionTree spec editionType =
    buildTypeTree spec editionType |> Zipper.fromTree |> buildEditionNode


buildInitialTree : S.Spec -> EditionTree
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
