module Editor.UI.Main exposing (main)

import Dict
import Maybe.Extra as Maybe
import Basics.Extra exposing (flip)

import Browser
import Http
import Url.Builder as UB
import Json.Encode as JE
import Json.Decode as JD
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode exposing (Value)
import List.Nonempty as NE exposing (Nonempty)
import Cmd.Extra as CX

import ParserExtra
import AceEditor as Ace
import HtmlExtra as HX
import Common exposing (expectJsonMessage)
import Util exposing (unwraperror)
import Plotter exposing
    ( Series
    , seriesdecoder
    , scatterplot
    , plotargs
    )

import Editor.Type exposing (getCode, ReturnTypeStr)
import Editor.UI.Tree as Tree
import Editor.UI.CodeEditor as CodeEditor


type Msg
    = CodeEditorMsg CodeEditor.Msg
    | EditionTreeMsg Tree.Msg
    | UpdateName String
    | OnSave
    | SaveDone (Result String String)
    | GotPlotData (Result Http.Error String)


type alias Model =
    { urlPrefix : String
    , codeEditor : CodeEditor.Model
    , editionTree : Tree.Model
    , formulaName : Maybe String
    , lastFormulaCode : Maybe String
    , savedFormulaCode : Maybe String
    , saveErrMess : Maybe String
    , plotData : Series
    , plotErrMess : Maybe String
    }


saveFormula : Model -> String -> String -> Cmd Msg
saveFormula {urlPrefix} name code = Http.request
    { method = "PATCH"
    , headers = []
    , url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "formula" ]
        []
    , body = Http.jsonBody (JE.object
        [ ( "name", JE.string name )
        , ( "text", JE.string code )
        , ( "reject_unknown", JE.bool True )
        ]
    )
    , expect = expectJsonMessage SaveDone JD.string
    , timeout = Nothing
    , tracker = Nothing
    }

getPlotData : Model -> Cmd Msg
getPlotData {urlPrefix, codeEditor} =
    case codeEditor.formulaCode of
        Just code -> Http.get
            { url = UB.crossOrigin
                urlPrefix
                [ "tsformula", "try" ]
                [ UB.string "formula" code ]
            , expect = Http.expectString GotPlotData
            }

        _ -> Cmd.none

updateLastFormula : Model -> String -> Model
updateLastFormula model code =
    if not (Tree.hasErrors model.editionTree) then
        { model | lastFormulaCode = Just code }
    else
        model

postUpdate : Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
postUpdate msg (model, cmd) = case msg of
    EditionTreeMsg (Tree.Edit code) ->
        (updateLastFormula model code, cmd)

    CodeEditorMsg (CodeEditor.Render code) ->
        (updateLastFormula model code, cmd)

    _ ->
        (model, cmd)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = postUpdate msg <| case msg of
    CodeEditorMsg (CodeEditor.UserEdited code) ->
        update (Tree.Edit code |> EditionTreeMsg) model

    CodeEditorMsg x -> Tuple.mapBoth
        (\m -> { model | codeEditor = m })
        (Cmd.map CodeEditorMsg)
        (CodeEditor.update x model.codeEditor)

    EditionTreeMsg (Tree.TreeEdited code) ->
        update (CodeEditor.Render code |> CodeEditorMsg) model

    EditionTreeMsg x -> Tuple.mapBoth
        (\m -> { model | editionTree = m })
        (Cmd.map EditionTreeMsg)
        (Tree.update x model.editionTree)

    UpdateName "" ->
        { model | formulaName = Nothing } |> CX.withNoCmd

    UpdateName formulaName ->
        { model | formulaName = Just formulaName } |> CX.withNoCmd

    OnSave ->
        ( model
        , Maybe.map2
            (saveFormula model)
            model.formulaName
            model.lastFormulaCode
            |> Maybe.withDefault Cmd.none
        )

    SaveDone (Ok _) ->
        { model
            | savedFormulaCode = model.lastFormulaCode
            , saveErrMess = Nothing
        } |> CX.withNoCmd

    SaveDone (Err s) ->
        { model
            | saveErrMess = Just s
        } |> CX.withNoCmd

    GotPlotData (Ok raw) -> case JD.decodeString seriesdecoder raw of
        Ok val ->
            { model
                | plotData = val
                , plotErrMess = Nothing
            }
            |> CX.withNoCmd

        Err err ->
            { model
                | plotData = Dict.empty
                , plotErrMess = Just (JD.errorToString err)
            }
            |> CX.withNoCmd

    GotPlotData (Err err) ->
        { model
            | plotData = Dict.empty
            , plotErrMess = Just (unwraperror err)
        }
        |> CX.withNoCmd


canSave : Model -> Bool
canSave model =
    (model.codeEditor.mode == CodeEditor.ReadOnly) &&
    (not <| Tree.hasErrors model.editionTree) &&
    (model.savedFormulaCode /= model.lastFormulaCode)

viewSave : Model -> Html Msg
viewSave {formulaName, saveErrMess} = H.div
    [ HA.class "container-fluid mt-2" ]
    [ H.input
        [ HA.class "w-75"
        , HA.value <| Maybe.withDefault "" formulaName
        , HE.onInput UpdateName
        ]
        []
    , H.button
        [ HA.class "btn btn-primary ml-2"
        , HA.disabled (Maybe.isNothing formulaName)
        , HE.onClick OnSave
        ]
        [ H.text "Save As" ]
    , viewError saveErrMess
    ]

viewError : Maybe String -> Html msg
viewError mStr = HX.viewMaybe
    (\x -> H.span [ HA.class "error" ] [ H.text x ])
    mStr

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

viewEditor : Model -> Html Msg
viewEditor model =
    let
        annotations =
            Tree.getParserErrors model.editionTree  |> Maybe.map getAnnotations

    in H.article [ HA.class "main" ]
    [ H.div
        [ HA.class "code_left" ]
        [ CodeEditor.viewEdition model.codeEditor annotations
            |> H.map CodeEditorMsg
        , Tree.viewErrors model.editionTree
        , CodeEditor.viewLastValid model.codeEditor model.lastFormulaCode
            |> H.map CodeEditorMsg
        ]
    , H.div
        [ HA.class "code_right" ]
        [ H.map EditionTreeMsg (Tree.view model.editionTree) ]
    ]

viewPlot : Model -> Html Msg
viewPlot {formulaName, plotData, plotErrMess} =
    let
        name = Maybe.withDefault "editor.code" formulaName
        plot = scatterplot
            name
            (Dict.keys plotData)
            (Dict.values plotData)
            "lines"
            Plotter.defaultoptions
        args = plotargs "plot" [plot] name
    in
    -- the "plot-figure" node is pre-built in the template side
    -- (html component)
    H.section
        []
        [ H.node "plot-figure" [ HA.attribute "args" args ] []
        , viewError plotErrMess
        ]

view : Model -> Html Msg
view model =
    H.div
        [ HA.class "main-content formula_editor" ]
        [ H.h1 [ HA.class "page-title" ] [ H.text "Formula editor" ]
        , Tree.viewSpecErrors model.editionTree
        , HX.viewIf (canSave model) (viewSave model)
        , viewEditor model
        , H.div [ HA.id "plot" ] []
        , viewPlot model
        ]


type alias Formula =
    { name : String
    , code : String
    }


type alias Flags =
    { urlPrefix : String
    , jsonSpec : Value
    , formula : Maybe Formula
    , returnTypeStr : ReturnTypeStr
    }


init : Flags -> ( Model, Cmd Msg )
init { urlPrefix, jsonSpec, formula, returnTypeStr } =
    let
        (editionTree, treeCmd) = Tree.init
            { urlPrefix = urlPrefix
            , jsonSpec = jsonSpec
            , formulaCode = Maybe.map .code formula
            , returnTypeStr = returnTypeStr
            }

        code = Just <| getCode editionTree.editor.currentFormula
    in
    { urlPrefix = urlPrefix
    , codeEditor = CodeEditor.init
    , editionTree = editionTree
    , formulaName = Maybe.map .name formula
    , lastFormulaCode = code
    , savedFormulaCode = code
    , saveErrMess = Nothing
    , plotData = Dict.empty
    , plotErrMess = Nothing
    }
    |> CX.withCmd (Cmd.batch
        [ (Cmd.map EditionTreeMsg <| Tree.sendTreeEdited editionTree)
        , (Cmd.map EditionTreeMsg treeCmd)
        ])

main : Program Flags Model Msg
main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }
