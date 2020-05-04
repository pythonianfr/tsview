module TsView.Formula.EditionTree.Update exposing (update, updateZipper)

import Cmd.Extra exposing (withNoCmd)
import Either exposing (Either(..))
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.))
import Tree.Zipper as Zipper exposing (Zipper)
import TsView.Formula.EditionTree.Type as ET exposing (EditionTree, Model, Msg)
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (sendCmd, valueParser)


readInput : S.InputType -> String -> ET.Input -> ET.Input
readInput inputType s input =
    let
        errMess x =
            "Could not convert \"" ++ x ++ "\" as " ++ S.strInputType inputType
    in
    if s == "" then
        ET.emptyInput

    else
        Tuple.pair s <|
            case inputType of
                S.String ->
                    Right <| S.StringValue s

                S.Timestamp ->
                    Right <| S.TimestampValue s

                S.SearchString ->
                    Right <| S.StringValue s

                _ ->
                    Parser.run (valueParser inputType |. Parser.end) s
                        |> Either.fromResult
                        |> Either.map Tuple.second
                        |> Either.voidLeft (errMess s)


type alias ZEN =
    Zipper ET.EditionNode


editionTreeFromZipper : S.Spec -> ZEN -> EditionTree
editionTreeFromZipper spec =
    Zipper.label >> .editionType >> ET.buildEditionTree spec


updateEditionType : Maybe ET.EditionType -> S.Spec -> ZEN -> ZEN
updateEditionType editionType spec zipper =
    let
        tree : Maybe EditionTree
        tree =
            Maybe.map (ET.buildEditionTree spec) editionType
    in
    Maybe.map2 Zipper.replaceTree tree (Zipper.firstChild zipper)
        |> Maybe.withDefault zipper


updateOperator : S.BaseType -> String -> S.Spec -> ZEN -> ZEN
updateOperator baseType s spec zipper =
    let
        editionType : Maybe ET.EditionType
        editionType =
            S.getOperator baseType s spec
                |> Maybe.map (ET.fromSpecOperator >> ET.OperatorT)
    in
    updateEditionType editionType spec zipper


updateInputType : S.InputType -> S.Spec -> ZEN -> ZEN
updateInputType inputType spec zipper =
    let
        editionType : ET.EditionType
        editionType =
            S.BaseInput inputType |> S.ExpBaseType |> ET.ExpTypeT
    in
    updateEditionType (Just editionType) spec zipper


updateFromString : S.Spec -> String -> ZEN -> ZEN
updateFromString spec stringValue zipper =
    let
        n =
            Zipper.label zipper
    in
    case n.editionType of
        ET.SelectorT baseType ->
            updateOperator baseType stringValue spec zipper

        ET.InputSelectorT inputType ->
            if S.strInputType inputType == stringValue then
                updateInputType inputType spec zipper

            else
                updateOperator (S.BaseInput inputType) stringValue spec zipper

        ET.ReturnTypeT xs ->
            let
                editionType =
                    S.matchBaseType xs stringValue
                        |> Maybe.map (ET.probeSelector spec)
            in
            updateEditionType editionType spec zipper

        ET.ExpTypeT (S.Union xs) ->
            let
                editionType =
                    S.matchExpType xs stringValue
                        |> Maybe.map (ET.probeArgSelector spec)
            in
            updateEditionType editionType spec zipper

        ET.ExpTypeT (S.ExpBaseType (S.BaseInput inputType)) ->
            Zipper.mapLabel
                (\x -> { x | input = readInput inputType stringValue x.input })
                zipper

        _ ->
            zipper


updateZipper : S.Spec -> ET.EditAction -> ZEN -> ZEN
updateZipper spec editAction zipper =
    case editAction of
        ET.ReadInput s ->
            updateFromString spec s zipper

        ET.ListAdd ->
            let
                appendChild : ZEN -> ZEN
                appendChild zipperChild =
                    Zipper.append
                        (editionTreeFromZipper spec zipperChild)
                        zipperChild

                defaultList : ZEN
                defaultList =
                    Zipper.replaceTree
                        (editionTreeFromZipper spec zipper)
                        zipper
            in
            Maybe.map appendChild (Zipper.lastChild zipper)
                |> Maybe.withDefault defaultList

        ET.ListRemove ->
            Zipper.removeTree zipper |> Maybe.withDefault zipper


toggleIsOpen : ET.EditionNode -> ET.EditionNode
toggleIsOpen n =
    let
        flags =
            n.editionFlags
    in
    { n | editionFlags = { flags | isOpen = not flags.isOpen } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newTreeModel zipper =
            { model | tree = Zipper.toTree zipper }
    in
    case msg of
        ET.Edit tree ->
            { model | tree = tree } |> withNoCmd

        ET.ToggleNode z ->
            Zipper.mapLabel toggleIsOpen z |> newTreeModel |> withNoCmd

        ET.EditNode z a ->
            let
                newModel =
                    updateZipper model.spec a z |> newTreeModel
            in
            ( newModel, sendCmd ET.RenderFormula newModel.tree )

        ET.RenderFormula _ ->
            model |> withNoCmd
