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
import Json.Encode as JE
import Json.Decode exposing (Value)
import List.Nonempty as NE exposing (Nonempty)
import Debouncer.Basic as Debouncer
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

import Editor.Type exposing (ReturnTypeStr)
import Editor.UI.Widget as Widget


type Msg
    = WidgetMsg Widget.Msg
    | UpdateName String
    | OnSave
    | SaveDone (Result String String)
    | DebouncePlotData (Debouncer.Msg ())
    | GotPlotData (Result Http.Error String)


type alias Model =
    { urlPrefix : String
    , editorWidget : Widget.Model
    , formulaName : Maybe String
    , lastFormulaCode : Maybe String
    , savedFormulaCode : Maybe String
    , saveErrMess : Maybe String
    , debouncer : Debouncer.Debouncer () ()
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
getPlotData {urlPrefix, lastFormulaCode} =
    case lastFormulaCode of
        Just code -> Http.get
            { url = UB.crossOrigin
                urlPrefix
                [ "tsformula", "try" ]
                [ UB.string "formula" code ]
            , expect = Http.expectString GotPlotData
            }

        _ -> JE.object []
            |> JE.encode 0
            |> Ok
            |> Util.sendCmd GotPlotData

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    WidgetMsg (Widget.NewFormula formula) ->
        { model | lastFormulaCode = formula }
            |> CX.withCmd
                (Util.sendCmd DebouncePlotData <|  Debouncer.provideInput ())

    WidgetMsg x -> Tuple.mapBoth
        (\m -> { model | editorWidget = m })
        (Cmd.map WidgetMsg)
        (Widget.update x model.editorWidget)

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

    DebouncePlotData debMsg ->
        let
            (debModel, debCmd, emitted) =
                Debouncer.update debMsg model.debouncer

            cmd = case emitted of
                Just _ -> getPlotData model

                Nothing -> Cmd.none
        in
        { model | debouncer = debModel }
            |> CX.withCmd (Cmd.batch [ cmd, Cmd.map DebouncePlotData debCmd ])

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
    (Widget.isReadOnly model.editorWidget) &&
    (Maybe.unwrap
        False
        (\current -> model.savedFormulaCode /= Just current)
        model.lastFormulaCode
    )

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
        , Widget.viewSpecErrors model.editorWidget
        , HX.viewIf (canSave model) (viewSave model)
        , Widget.view model.editorWidget |> H.map WidgetMsg
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
        (wid, cmd) = Widget.init
            { urlPrefix = urlPrefix
            , jsonSpec = jsonSpec
            , formulaCode = Maybe.map .code formula
            , returnTypeStr = returnTypeStr
            }

        code = Widget.getFormula wid
    in
    { urlPrefix = urlPrefix
    , editorWidget = wid
    , formulaName = Maybe.map .name formula
    , lastFormulaCode = code
    , savedFormulaCode = code
    , saveErrMess = Nothing
    , debouncer = Debouncer.debounce (Debouncer.fromSeconds 2)
        |> Debouncer.toDebouncer
    , plotData = Dict.empty
    , plotErrMess = Nothing
    }
    |> CX.withCmd (Cmd.map WidgetMsg cmd)

main : Program Flags Model Msg
main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }
