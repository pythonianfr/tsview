module Editor.UI.Widget exposing (..)

import Maybe.Extra as Maybe
import Basics.Extra exposing (flip)

import Either
import Cmd.Extra as CX
import List.Nonempty as NE exposing (Nonempty)

import Html as H exposing (Html)
import Html.Attributes as HA

import ParserExtra
import UndoList as UL
import AceEditor as Ace
import Util exposing (sendCmd)

import Editor.Type exposing (FormulaCode, getCode)
import Editor.UI.UndoRedo as UR
import Editor.UI.Tree as Tree
import Editor.UI.CodeEditor as CodeEditor
import UndoListExtra exposing (newSafeConcise)


type Msg
    = CodeEditorMsg CodeEditor.Msg
    | EditionTreeMsg Tree.Msg
    | NewFormula (Maybe FormulaCode)


type alias SavedModel =
    { codeEditor : CodeEditor.Model
    , editionTree : Tree.Model
    }

type alias Model =
    { savedModel : SavedModel
    , undoList : UR.UndoList SavedModel
    }


type alias Flags = Tree.Flags


init : Flags -> (Model, Cmd Msg)
init flags =
    let
        (editionTree, treeCmd) = Tree.init flags
        code = getCode editionTree.editor.currentFormula

        savedModel =
            { codeEditor = CodeEditor.init
            , editionTree = editionTree
            }
    in
    { savedModel = savedModel
    , undoList = UL.fresh savedModel
    }
    |> update (CodeEditor.Render code |> CodeEditorMsg)
    |> Tuple.first
    |> CX.withCmd (Cmd.map EditionTreeMsg treeCmd)

viewSpecErrors : Model -> Html msg
viewSpecErrors {savedModel} = Tree.viewSpecErrors savedModel.editionTree

isReadOnly : Model -> Bool
isReadOnly {savedModel} = savedModel.codeEditor.isReadOnly

getFormula : Model -> Maybe FormulaCode
getFormula {savedModel} =
    Either.toMaybe savedModel.editionTree.editor.currentFormula
        |> Maybe.map Tuple.first

setFormula : Maybe FormulaCode -> Model -> (Model, Cmd Msg)
setFormula code {savedModel, undoList} =
    let
        newTree =  Tree.setFormula code savedModel.editionTree
    in
    { savedModel =
        { codeEditor = CodeEditor.init
        , editionTree = newTree
        }
    , undoList = undoList
    }
    |> CX.withCmd (Cmd.map EditionTreeMsg <| Tree.sendTreeEdited newTree)

sendNew : SavedModel -> Cmd Msg
sendNew {editionTree} = editionTree.editor.currentFormula
    |> Either.toMaybe
    |> Maybe.map Tuple.first
    |> sendCmd NewFormula

updateSaved : Msg -> SavedModel -> ( SavedModel, Cmd Msg )
updateSaved msg model = case msg of
    CodeEditorMsg (CodeEditor.UserEdited code) ->
        updateSaved (Tree.Edit code |> EditionTreeMsg) model
            |> \(m, cmd) -> (m, Cmd.batch [cmd, sendNew m])

    CodeEditorMsg x -> Tuple.mapBoth
        (\m -> { model | codeEditor = m })
        (Cmd.map CodeEditorMsg)
        (CodeEditor.update x model.codeEditor)

    EditionTreeMsg (Tree.TreeEdited code) ->
        updateSaved (CodeEditor.Render code |> CodeEditorMsg) model
            |> \(m, cmd) -> (m, Cmd.batch [cmd, sendNew m])

    EditionTreeMsg x -> Tuple.mapBoth
        (\m -> { model | editionTree = m })
        (Cmd.map EditionTreeMsg)
        (Tree.update x model.editionTree)

    NewFormula _ -> (model, Cmd.none)

mergeModel : SavedModel -> SavedModel -> SavedModel
mergeModel user current =
    { codeEditor = CodeEditor.mergeModel user.codeEditor current.codeEditor
    , editionTree = current.editionTree
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg {savedModel, undoList} =
    let
        (newModel, cmd) = updateSaved msg savedModel

        undoRedo newUndoList =
            ( Model
                (mergeModel newModel newUndoList.present)
                newUndoList
            , cmd )

    in case msg of
        NewFormula (Just _) ->
            ( Model newModel (newSafeConcise newModel undoList), cmd )

        CodeEditorMsg (CodeEditor.UndoRedoMsg UR.Undo) ->
            UL.undo undoList |> undoRedo

        CodeEditorMsg (CodeEditor.UndoRedoMsg UR.Redo) ->
            UL.redo undoList |> undoRedo

        EditionTreeMsg (Tree.UndoRedoMsg UR.Undo) ->
            UL.undo undoList |> undoRedo

        EditionTreeMsg (Tree.UndoRedoMsg UR.Redo) ->
            UL.redo undoList |> undoRedo

        _ ->
            ( Model newModel undoList, cmd )

convertAnnotation :
        Ace.AnnotationType -> ParserExtra.Annotation -> Ace.Annotation
convertAnnotation annotationType {rowPos, colPos, errMess} =
    { annotationType = annotationType
    , rowPos = rowPos
    , colPos = colPos
    , message = errMess
    }

getAnnotations : ParserExtra.ParserErrors -> Ace.Annotations
getAnnotations xs = flip NE.concatMap xs <| \{annotation, contextStack} ->
    Maybe.unwrap
        []
        (NE.map (convertAnnotation Ace.Info) >> NE.toList)
        contextStack
        |> NE.Nonempty (convertAnnotation Ace.Warning annotation)

view : Model -> Html Msg
view ({savedModel, undoList} as model) =
    let
        annotations =
            Tree.getParserErrors savedModel.editionTree  |> Maybe.map getAnnotations

    in H.article [ HA.class "main formula_editor" ]
    [ H.div
        [ HA.class "code_left" ]
        [ CodeEditor.viewEdition savedModel.codeEditor undoList annotations
            |> H.map CodeEditorMsg
        , Tree.viewErrors savedModel.editionTree
        ]
    , H.div
        [ HA.class "code_right" ]
        [ H.map EditionTreeMsg <|
            Tree.view savedModel.editionTree undoList (isReadOnly model)
        ]
    ]
