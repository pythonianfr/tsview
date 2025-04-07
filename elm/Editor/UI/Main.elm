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
import UndoList as UL
import Optics.Core as O exposing (o)
import OpticsExtra as OE
import Common exposing (expectJsonMessage)
import Util exposing (unwraperror)
import Plotter exposing
    ( defaultLayoutOptions
    , defaultConfigOptions
    , Series
    , seriesdecoder
    , scatterplot
    , serializedPlotArgs
    )

import Editor.Type exposing (ReturnTypeStr)
import Editor.UI.Widget as Widget
import UndoListExtra exposing (newSafeConcise)


type UndoMsg
    = Undo
    | Redo


type Msg
    = WidgetMsg Widget.Msg
    | UpdateName String
    | OnSave
    | SaveDone (Result String String)
    | DebouncePlotData (Debouncer.Msg ())
    | GotFormulaNames (Result Http.Error (List String))
    | GotPlotData (Result Http.Error String)
    | UndoMsg UndoMsg


type alias PlotData =
    { debouncer : Debouncer.Debouncer () ()
    , loading : Bool
    , series : Series
    , errMess : Maybe String
    }

type alias Register =
    { current : Maybe String
    , saved : Maybe String
    }

type alias Model =
    { urlPrefix : String
    , editorWidget : Widget.Model
    , formulaNameRegister : Register
    , formulaCodeRegister : Register
    , formulaNames : List String
    , saveErrMess : Maybe String
    , plotData : PlotData
    }

type alias SavedModel =
    { hasPlotData : Bool
    , savedModel : Model
    }

type alias UndoList =
    UL.UndoList SavedModel

type alias UndoModel =
    { model : Model
    , undoList : UndoList
    }

current_ : O.SimpleLens ls Register (Maybe String)
current_ = O.lens .current <| \s a -> {s | current = a}

saved_ : O.SimpleLens ls Register (Maybe String)
saved_ = O.lens .saved <| \s a -> {s | saved = a}

saveCurrent : Register -> Register
saveCurrent register = { register | saved = register.current }

formulaNameRegister_ : O.SimpleLens ls Model Register
formulaNameRegister_ = O.lens .formulaNameRegister <|
    \s a -> {s | formulaNameRegister = a}

formulaCodeRegister_ : O.SimpleLens ls Model Register
formulaCodeRegister_ = O.lens .formulaCodeRegister <|
    \s a -> {s | formulaCodeRegister = a}

plotData_ : O.SimpleLens ls Model PlotData
plotData_ = O.lens .plotData <| \s a -> {s | plotData = a}

loading_ : O.SimpleLens ls PlotData Bool
loading_ = O.lens .loading <| \s a -> {s | loading = a}

plotLoading_ : O.SimpleLens ls Model Bool
plotLoading_ = o plotData_ loading_

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

askPlotData : Model -> (Model, Cmd Msg)
askPlotData model =
    ( O.assign plotLoading_ True model
    , Util.sendCmd DebouncePlotData <|  Debouncer.provideInput ()
    )

getPlotData : Model -> Cmd Msg
getPlotData {urlPrefix, formulaCodeRegister} =
    case formulaCodeRegister.current of
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


updatePlotData : Result Http.Error String -> PlotData -> PlotData
updatePlotData res plotData = O.assign loading_ False <|
    case Result.map (JD.decodeString seriesdecoder) res of
        (Ok (Ok val)) ->
            { plotData
                | series = val
                , errMess = Nothing
            }

        (Ok (Err err)) ->
            { plotData
                | series = Dict.empty
                , errMess = Just (JD.errorToString err)
            }

        (Err err) ->
            { plotData
                | series = Dict.empty
                , errMess = Just (unwraperror err)
            }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    WidgetMsg (Widget.NewFormula formula) -> model
        |> O.assign (o formulaCodeRegister_ current_) formula
        |> askPlotData

    WidgetMsg x -> Tuple.mapBoth
        (\m -> { model | editorWidget = m })
        (Cmd.map WidgetMsg)
        (Widget.update x model.editorWidget)

    UpdateName "" -> model
        |> O.assign (o formulaNameRegister_ current_) Nothing
        |> CX.withNoCmd

    UpdateName formulaName -> model
        |> O.assign (o formulaNameRegister_ current_) (Just formulaName)
        |> CX.withNoCmd

    OnSave ->
        ( model
        , Maybe.map2
            (saveFormula model)
            model.formulaNameRegister.current
            model.formulaCodeRegister.current
            |> Maybe.withDefault Cmd.none
        )

    SaveDone (Ok _) -> { model | saveErrMess = Nothing }
        |> O.over formulaNameRegister_ saveCurrent
        |> O.over formulaCodeRegister_ saveCurrent
        |> CX.withCmd (getFormulaNames model.urlPrefix)

    SaveDone (Err s) -> { model | saveErrMess = Just s }
        |> CX.withNoCmd

    DebouncePlotData debMsg ->
        let
            (debModel, debCmd, emitted) =
                Debouncer.update debMsg model.plotData.debouncer

            cmd = case emitted of
                Just _ -> getPlotData model

                Nothing -> Cmd.none
        in
        O.over plotData_ (\x -> { x | debouncer = debModel }) model
            |> CX.withCmd (Cmd.batch [ cmd, Cmd.map DebouncePlotData debCmd ])

    GotFormulaNames (Ok xs) ->
        { model | formulaNames = xs } |> CX.withNoCmd

    GotFormulaNames (Err _) ->
        model |> CX.withNoCmd

    GotPlotData res ->
        O.over plotData_ (updatePlotData res) model |> CX.withNoCmd

    UndoMsg _ ->
        ( model, Cmd.none )

undoUpdate : Msg -> UndoModel -> (UndoModel, Cmd Msg)
undoUpdate msg {model, undoList} =
    let
        (newModel, cmd) = update msg model

        storeNew hasPlotData =
            O.over plotData_ (\x -> { x | series = Dict.empty}) newModel
                |> SavedModel hasPlotData
                |> flip newSafeConcise undoList
                |> UndoModel newModel
                |> CX.withCmd cmd

        undoRedo newUndoList =
            let {hasPlotData, savedModel} = newUndoList.present
            in O.over OE.first_ (flip UndoModel newUndoList) <|
                if hasPlotData then
                    askPlotData savedModel
                else
                    (savedModel, Cmd.none)

    in case msg of
        WidgetMsg (Widget.NewFormula _) -> storeNew True

        UpdateName _ -> storeNew False

        SaveDone _ -> storeNew True

        UndoMsg Undo -> UL.undo undoList |> undoRedo

        UndoMsg Redo -> UL.redo undoList |> undoRedo

        _ -> (UndoModel newModel undoList, cmd)

canSave : Model -> Bool
canSave model =
    (Widget.isReadOnly model.editorWidget) &&
    (Maybe.unwrap
        False
        (\current -> model.formulaCodeRegister.saved /= Just current)
        model.formulaCodeRegister.current
    )

viewSave : Model -> Html Msg
viewSave {formulaNameRegister, formulaNames, saveErrMess} =
    let formulaName = formulaNameRegister.current

        isSavedFormulaName = Maybe.unwrap
            False
            (\x -> List.member x formulaNames)
            formulaName

    in H.div
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
            [ H.text <| if isSavedFormulaName then "Update" else "Create" ]
        , viewError saveErrMess
        ]

viewError : Maybe String -> Html msg
viewError mStr = HX.viewMaybe
    (\x -> H.span [ HA.class "error" ] [ H.text x ])
    mStr

viewPlot : Model -> Html Msg
viewPlot {formulaNameRegister, plotData} =
    let
        name = Maybe.withDefault "" formulaNameRegister.current
        plot = scatterplot
            name
            (Dict.keys plotData.series)
            (Dict.values plotData.series)
            "lines"
            Plotter.defaultTraceOptions
        args = serializedPlotArgs
                "plot"
                [plot]
                { defaultLayoutOptions | title = Just name }
                defaultConfigOptions
    in
    -- the "plot-figure" node is pre-built in the template side
    -- (html component)
    H.section
        []
        [ H.node "plot-figure" [ HA.attribute "args" args ] []
        , viewError plotData.errMess
        ]

viewSeriesInfo : Model -> Html msg
viewSeriesInfo model =
    flip HX.viewMaybe model.formulaNameRegister.saved <| \name ->
        let
            href = UB.crossOrigin
                model.urlPrefix
                [ "tsinfo" ]
                [ UB.string "name" name ]

        in H.a [ HA.href href  ] [ H.text "series info" ]

makeUndoButton : UndoMsg -> UndoList -> Html Msg
makeUndoButton msg undoList =
    let
        (code, title, enabled) = case msg of
            Undo -> (10226, "Undo", UL.hasPast undoList)
                -- ANTICLOCKWISE GAPPED CIRCLE ARROW

            Redo -> (10227, "Redo", UL.hasFuture undoList)
                -- CLOCKWISE GAPPED CIRCLE ARROW

    in H.div
        [ HA.class "p-2" ]
        [ H.button
            [ HA.title title
            , HA.disabled (not enabled)
            , HE.onClick (UndoMsg msg)
            ]
            [ H.text <| Util.fromCharCode code ]
        ]

--view : UndoModel -> Html Msg
--view {model, undoList} =
view : Model -> Html Msg
view model =
    H.div
        [ HA.class "main-content formula_editor" ] <|
        [ H.div
            [ HA.class "d-flex" ]
            [ H.h1
                [ HA.class "mr-auto p-2 page-title" ]
                [ H.text "Formula editor" ]
            , H.div [ HA.class "p-2" ] [ viewSeriesInfo model ]
            ]
        , H.div
            [ HA.class "d-flex" ]
            --[ makeUndoButton Undo undoList
            --, makeUndoButton Redo undoList
            [ HX.viewIf (canSave model) (viewSave model)
            ]
        , Widget.viewSpecErrors model.editorWidget
        , Widget.view model.editorWidget |> H.map WidgetMsg
        ] ++
        if O.get plotLoading_ model then
            [ H.img
                [ HA.class "img_loading"
                , HA.src "./tsview_static/loading_wheel.gif"
                ]
                []
            ]
        else
            [ H.div [ HA.id "plot" ] []
            , viewPlot model
            ]


getFormulaNames : String -> Cmd Msg
getFormulaNames urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "find" ]
        [ UB.string "query" "(by.formula '')"
        , UB.string "meta" "false"
        ]
    , expect = Http.expectJson
        GotFormulaNames
        (JD.list (JD.field "name" JD.string))
    }

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

        formulaName = Maybe.map .name formula
        code = Widget.getFormula wid
    in
    { urlPrefix = urlPrefix
    , editorWidget = wid
    , formulaNameRegister =
        { current = formulaName
        , saved = formulaName
        }
    , formulaCodeRegister =
        { current = code
        , saved = code
        }
    , formulaNames = []
    , saveErrMess = Nothing
    , plotData =
        { debouncer = Debouncer.debounce (Debouncer.fromSeconds 2)
            |> Debouncer.toDebouncer
        , loading = False
        , series = Dict.empty
        , errMess = Nothing
        }
    }
    |> CX.withCmd (Cmd.batch
        [ Cmd.map WidgetMsg cmd
        , getFormulaNames urlPrefix
        ]
    )

initModel : Model -> UndoModel
initModel model =
    SavedModel True model
        |> UL.fresh
        |> UndoModel model

-- main : Program Flags UndoModel Msg
main : Program Flags Model Msg
main = Browser.element
    { init = init -- >> Tuple.mapFirst initModel
    -- , update = undoUpdate
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }
