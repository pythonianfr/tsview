module TsView.Formula.Main exposing (main)

import Browser
import Either
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode exposing (Value)
import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.CodeEditor as CodeEditor
import TsView.Formula.EditionTree.Main as EditionTree
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.Spec.Parser exposing (parseSpecValue)


type Msg
    = CodeEditorMsg CodeEditor.Msg
    | EditionTreeMsg EditionTree.Msg


type alias Model =
    { urlPrefix : String
    , codeEditor : CodeEditor.Model
    , editionTree : EditionTree.Model
    , errors : Maybe (Nonempty String)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeEditorMsg (CodeEditor.ParsedFormula tree) ->
            update (ET.Edit tree |> EditionTreeMsg) model

        CodeEditorMsg x ->
            Tuple.mapBoth
                (\m -> { model | codeEditor = m })
                (Cmd.map CodeEditorMsg)
                (CodeEditor.update x model.codeEditor)

        EditionTreeMsg (ET.RenderFormula tree) ->
            update (CodeEditor.Render tree |> CodeEditorMsg) model

        EditionTreeMsg x ->
            Tuple.mapBoth
                (\m -> { model | editionTree = m })
                (Cmd.map EditionTreeMsg)
                (EditionTree.update x model.editionTree)


view : Model -> Html Msg
view model =
    H.article [ A.class "main" ]
        [ H.map CodeEditorMsg (CodeEditor.view model.codeEditor)
        , H.map EditionTreeMsg (EditionTree.view model.editionTree)
        ]


type alias Flags =
    { urlPrefix : String
    , jsonSpec : Value
    , formula : Maybe CodeEditor.Formula
    }


init : Flags -> ( Model, Cmd Msg )
init { urlPrefix, jsonSpec, formula } =
    let
        initModel ( spec, errors ) =
            let
                ( codeModel, codeCmd ) =
                    CodeEditor.init urlPrefix spec formula
            in
            ( Model
                urlPrefix
                codeModel
                (EditionTree.init spec)
                errors
            , Cmd.map CodeEditorMsg codeCmd
            )
    in
    parseSpecValue jsonSpec
        |> Either.unpack
            (\( spec, errors ) -> ( spec, Just errors ))
            (\spec -> ( spec, Nothing ))
        |> initModel


main : Program Flags Model Msg
main =
    let
        sub model =
            Sub.none
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = sub
        }
