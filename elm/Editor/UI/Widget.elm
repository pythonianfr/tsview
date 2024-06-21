module Editor.UI.Widget exposing (..)

import Maybe.Extra as Maybe
import Basics.Extra exposing (flip)

import Either
import Cmd.Extra as CX
import List.Nonempty as NE exposing (Nonempty)

import Html as H exposing (Html)
import Html.Attributes as HA

import ParserExtra
import AceEditor as Ace
import Util exposing (sendCmd)

import Editor.Type exposing (FormulaCode)
import Editor.UI.Tree as Tree
import Editor.UI.CodeEditor as CodeEditor


type Msg
    = CodeEditorMsg CodeEditor.Msg
    | EditionTreeMsg Tree.Msg
    | NewFormula (Maybe FormulaCode)


type alias Model =
    { codeEditor : CodeEditor.Model
    , editionTree : Tree.Model
    }


type alias Flags = Tree.Flags


init : Flags -> (Model, Cmd Msg)
init flags =
    let (editionTree, treeCmd) = Tree.init flags
    in
    { codeEditor = CodeEditor.init
    , editionTree = editionTree
    }
    |> CX.withCmd (Cmd.batch
        [ (Cmd.map EditionTreeMsg <| Tree.sendTreeEdited editionTree)
        , (Cmd.map EditionTreeMsg treeCmd)
        ])

viewSpecErrors : Model -> Html msg
viewSpecErrors {editionTree} = Tree.viewSpecErrors editionTree

isReadOnly : Model -> Bool
isReadOnly model = model.codeEditor.mode == CodeEditor.ReadOnly

getFormula : Model -> Maybe FormulaCode
getFormula {editionTree} =
    Either.toMaybe editionTree.editor.currentFormula
        |> Maybe.map Tuple.first

setFormula : Maybe FormulaCode -> Model -> (Model, Cmd Msg)
setFormula code {editionTree} =
    let
        newTree =  Tree.setFormula code editionTree
    in
    { codeEditor = CodeEditor.init
    , editionTree = newTree
    }
    |> CX.withCmd (Cmd.map EditionTreeMsg <| Tree.sendTreeEdited newTree)

sendNew : Model -> Cmd Msg
sendNew {editionTree} = editionTree.editor.currentFormula
    |> Either.toMaybe
    |> Maybe.map Tuple.first
    |> sendCmd NewFormula

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    CodeEditorMsg (CodeEditor.UserEdited code) ->
        update (Tree.Edit code |> EditionTreeMsg) model
            |> \(m, cmd) -> (m, Cmd.batch [cmd, sendNew m])

    CodeEditorMsg x -> Tuple.mapBoth
        (\m -> { model | codeEditor = m })
        (Cmd.map CodeEditorMsg)
        (CodeEditor.update x model.codeEditor)

    EditionTreeMsg (Tree.TreeEdited code) ->
        update (CodeEditor.Render code |> CodeEditorMsg) model
            |> \(m, cmd) -> (m, Cmd.batch [cmd, sendNew m])

    EditionTreeMsg x -> Tuple.mapBoth
        (\m -> { model | editionTree = m })
        (Cmd.map EditionTreeMsg)
        (Tree.update x model.editionTree)

    NewFormula _ -> (model, Cmd.none)

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
view model =
    let
        annotations =
            Tree.getParserErrors model.editionTree  |> Maybe.map getAnnotations

    in H.article [ HA.class "main" ]
    [ H.div
        [ HA.class "code_left" ]
        [ CodeEditor.viewEdition model.codeEditor annotations
            |> H.map CodeEditorMsg
        , Tree.viewErrors model.editionTree
        ]
    , H.div
        [ HA.class "code_right" ]
        [ H.map EditionTreeMsg (Tree.view model.editionTree) ]
    ]
