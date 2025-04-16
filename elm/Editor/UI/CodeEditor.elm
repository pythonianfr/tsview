module Editor.UI.CodeEditor exposing (..)

import Maybe.Extra as Maybe
import Cmd.Extra as CX

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

import AceEditor
import HtmlExtra as HX
import Util exposing (sendCmd)

import Editor.Type as T
import Editor.UI.UndoRedo as UndoRedo


type Msg
    = SwitchMode Bool
    | AceEditorMsg AceEditor.Msg
    | Render T.FormulaCode
    | UserEdited T.FormulaCode
    | UndoRedoMsg UndoRedo.Msg


type alias Model =
    { formulaCode : Maybe String
    , isReadOnly : Bool
    , reload : Bool
    }


init : Model
init =
    { formulaCode = Nothing
    , isReadOnly = True
    , reload = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    SwitchMode isReadOnly ->
        { model | isReadOnly = isReadOnly } |> CX.withNoCmd

    AceEditorMsg (AceEditor.Edited code) ->
        { model | formulaCode = Just code, reload = False }
            |> CX.withCmd (sendCmd UserEdited code)

    Render code -> case model.isReadOnly of
        True -> { model | formulaCode = Just code } |> CX.withNoCmd
        -- Edition -> { model | formulaCode = Just code, reload = True } |> CX.withNoCmd
        False ->  model |> CX.withNoCmd

    UserEdited _ ->
        model |> CX.withNoCmd

    UndoRedoMsg _ ->
        model |> CX.withNoCmd

mergeModel : Model -> Model -> Model
mergeModel user current =
    { user | formulaCode = current.formulaCode, reload = True }

editorHeight =
    HA.attribute "style" "--min-height-editor: 36vh"

viewHeader : Bool -> UndoRedo.UndoList a -> Html Msg
viewHeader isReadOnly undoList =
    let
        ( newMode, sign ) = case isReadOnly of
            True -> ( False, "✎" )
            False -> ( True, "💾" )

    in H.header
    [ HA.class "code_left" ]
    [ H.span [] [ H.text "Formula edition " ]
    , H.a [ HE.onClick (SwitchMode newMode) ] [ H.text sign ]
    , HX.viewIf
        (not isReadOnly)
        (H.map UndoRedoMsg <| UndoRedo.renderButtons undoList)
    ]

viewEdition : Model -> UndoRedo.UndoList a -> Maybe AceEditor.Annotations -> Html Msg
viewEdition model undoList annotations =
    let code = Maybe.withDefault "" model.formulaCode
    in H.section [ HA.class "code_editor" ] <| case model.isReadOnly of
    True ->
        [ viewHeader model.isReadOnly undoList
        , H.div
            [ HA.class "code_left" ]
            [ AceEditor.readOnly_ annotations code ]
        ]

    False ->
        [ viewHeader model.isReadOnly undoList
        , H.div
            [ HA.class "code_left", editorHeight ]
            [ AceEditor.edit_ annotations code model.reload
                |> H.map AceEditorMsg
            ]
        ]

viewLastValid : Model -> Maybe String -> Html Msg
viewLastValid model code = case model.isReadOnly of
    True -> H.text ""
    False -> H.section [ HA.class "code_editor" ]
        [ H.header
            [ HA.class "code_left" ]
            [ H.span [] [ H.text "Last valid formula" ] ]
        , H.div
            [ HA.class "code_left" ]
            [ AceEditor.readOnly_ Nothing (Maybe.withDefault "" code)
            ]
        ]
