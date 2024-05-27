module Editor.UI.CodeEditor exposing (..)

import Maybe.Extra as Maybe
import Cmd.Extra as CX

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

import AceEditor
import Util exposing (sendCmd)

import Editor.Type as T


type Mode
    = ReadOnly
    | Edition


type Msg
    = SwitchMode Mode
    | AceEditorMsg AceEditor.Msg
    | Render T.FormulaCode
    | UserEdited T.FormulaCode


type alias Model =
    { formulaCode : Maybe String
    , mode : Mode
    , reload : Bool
    }


init : Model
init =
    { formulaCode = Nothing
    , mode = ReadOnly
    , reload = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    SwitchMode mode ->
        { model | mode = mode } |> CX.withNoCmd

    AceEditorMsg (AceEditor.Edited code) ->
        { model | formulaCode = Just code, reload = False }
            |> CX.withCmd (sendCmd UserEdited code)

    Render code -> case model.mode of
        ReadOnly -> { model | formulaCode = Just code } |> CX.withNoCmd
        -- Edition -> { model | formulaCode = Just code, reload = True } |> CX.withNoCmd
        Edition ->  model |> CX.withNoCmd

    UserEdited _ ->
        model |> CX.withNoCmd

editorHeight =
    HA.attribute "style" "--min-height-editor: 36vh"

viewHeader : Mode -> Html Msg
viewHeader mode =
    let
        ( newMode, sign ) = case mode of
            ReadOnly -> ( Edition, "✎" )
            Edition -> ( ReadOnly, "💾" )

    in H.header
    [ HA.class "code_left" ]
    [ H.span [] [ H.text "Formula edition " ]
    , H.a [ HE.onClick (SwitchMode newMode) ] [ H.text sign ]
    ]

viewEdition : Model -> Html Msg
viewEdition model =
    let
        code = Maybe.withDefault "" model.formulaCode

    in H.section [ HA.class "code_editor" ] <| case model.mode of
    ReadOnly ->
        [ viewHeader model.mode
        , H.div
            [ HA.class "code_left" ]
            [ AceEditor.readOnly_ code ]
        ]

    Edition ->
        [ viewHeader model.mode
        , H.div
            [ HA.class "code_left", editorHeight ]
            [ AceEditor.edit_ code model.reload |> H.map AceEditorMsg ]
        ]

viewLastValid : Model -> Maybe String -> Html Msg
viewLastValid model code = case model.mode of
    ReadOnly -> H.text ""
    Edition -> H.section [ HA.class "code_editor" ]
        [ H.header
            [ HA.class "code_left" ]
            [ H.span [] [ H.text "Last valid formula" ] ]
        , H.div
            [ HA.class "code_left" ]
            [ AceEditor.readOnly_ <| Maybe.withDefault "" code
            ]
        ]
