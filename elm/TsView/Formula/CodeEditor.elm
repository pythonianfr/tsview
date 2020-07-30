module TsView.Formula.CodeEditor exposing
    ( Formula
    , Model
    , Msg(..)
    , init
    , update
    , view
    )

import Cmd.Extra exposing (withNoCmd)
import Common exposing (expectJsonMessage)
import Dict exposing (Dict)
import Either
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Http
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Plotter exposing
    ( seriesdecoder
    , Series
    )
import TsView.AceEditor as AceEditor
import TsView.Formula.EditionTree.Parser exposing (parseFormula)
import TsView.Formula.EditionTree.Render exposing (renderString)
import TsView.Formula.EditionTree.Type as ET exposing (EditionTree)
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (icon, sendCmd)
import Url.Builder as UB
import Util as U


type alias Formula =
    { name : String
    , code : String
    }


type alias PartialFormula =
    { formula : Formula
    , errMess : Maybe String
    }


noFormula : Formula
noFormula =
    Formula "" ""


type State
    = ReadOnly
    | Edition


type Tab
    = Editor
    | Plot


type alias Model =
    { urlPrefix : String
    , tab : Tab
    , errors : List String
    , state : State
    , spec : S.Spec
    , current : PartialFormula
    , user : PartialFormula
    , reload : Bool
    -- plot
    , name : String
    , plotdata : Series
    }


type Msg
    = ParsedFormula EditionTree
    | Render EditionTree
    | ParseFormula String
    | AceEditorMsg AceEditor.Msg
    | ChangeState State
    | UpdateName String
    | UpdateUserFormula
    | OnSave
    | SaveDone (Result String String)
    | GotPlotData (Result Http.Error String)


updateName : String -> (Formula -> Formula)
updateName x s =
    { s | name = x }


updateCode : String -> (Formula -> Formula)
updateCode x s =
    { s | code = x }


updateFormula : (Formula -> Formula) -> (PartialFormula -> PartialFormula)
updateFormula modify s =
    { s | formula = modify s.formula }


updateErrMess : Maybe String -> (PartialFormula -> PartialFormula)
updateErrMess x s =
    { s | errMess = x }


updateState : State -> (Model -> Model)
updateState x s =
    { s | state = x }


updateCurrent : (PartialFormula -> PartialFormula) -> (Model -> Model)
updateCurrent modify s =
    { s | current = modify s.current }


updateUser : (PartialFormula -> PartialFormula) -> (Model -> Model)
updateUser modify s =
    { s | user = modify s.user }


updateCurrentCode : EditionTree -> (Model -> Model)
updateCurrentCode tree =
    let
        code =
            renderString tree
    in
    updateCode code |> updateFormula |> updateCurrent


getplotdata model =
    Http.get
        { url = UB.crossOrigin model.urlPrefix
              [ "tsformula", "try" ]
              [ UB.string "formula" model.current.formula.code ]
        , expect = Http.expectString GotPlotData
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr error =
            U.nocmd <| U.adderror model error
    in
    case msg of
        ParseFormula code ->
            let
                newModel =
                    (updateCode code |> updateFormula |> updateUser) model
            in
            Either.unpack
                (\s ->
                    let
                        f : Model -> Model
                        f =
                            updateState Edition
                                >> (updateErrMess (Just s) |> updateUser)
                    in
                    ( f newModel
                    , Cmd.none
                    )
                )
                (\tree ->
                    let
                        mod =
                            (updateCurrentCode tree
                                >> (updateErrMess Nothing |> updateUser)) model
                    in
                    ( mod
                    , Cmd.batch [ getplotdata mod
                                , sendCmd ParsedFormula tree
                                ]
                    )
                )
                (parseFormula model.spec code)

        Render tree ->
            updateCurrentCode tree model |> withNoCmd

        UpdateName s ->
            (updateName s |> updateFormula |> updateCurrent) model
                |> withNoCmd

        AceEditorMsg (AceEditor.Edited code) ->
            update (ParseFormula code) { model | reload = False }

        UpdateUserFormula ->
            let
                f =
                    always model.current.formula |> updateFormula |> updateUser
            in
            f { model | reload = True }
                |> update (ParseFormula model.current.formula.code)

        ChangeState state ->
            let
                f : Model -> Model
                f =
                    (always model.current.formula |> updateFormula |> updateUser)
                        >> (updateErrMess Nothing |> updateUser)
                        >> updateState state
            in
            f model |> withNoCmd

        OnSave ->
            let
                formula =
                    model.current.formula
            in
            ( model
            , Http.request
                { method = "PATCH"
                , headers = []
                , url =
                    UB.crossOrigin
                        model.urlPrefix
                        [ "api", "series", "formula" ]
                        []
                , body =
                    Http.jsonBody
                        (E.object
                            [ ( "name", E.string formula.name )
                            , ( "text", E.string formula.code )
                            , ( "reject_unknown", E.bool False )
                            , ( "force_update", E.bool True )
                            ]
                        )
                , expect = expectJsonMessage SaveDone D.string
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        SaveDone (Ok _) ->
            (updateErrMess Nothing |> updateCurrent) model |> withNoCmd

        SaveDone (Err s) ->
            (updateErrMess (Just s) |> updateCurrent) model |> withNoCmd

        GotPlotData (Ok rawdata) ->
            case D.decodeString seriesdecoder rawdata of
                Ok val ->
                    U.nocmd { model | plotdata = val }
                Err err ->
                    doerr <| D.errorToString err

        ParsedFormula _ ->
            model |> withNoCmd

        GotPlotData (Err err) ->
            doerr <| U.unwraperror err


init : String -> S.Spec -> Maybe Formula -> ( Model, Cmd Msg )
init urlPrefix spec initialFormulaM =
    let
        doInit model =
            Maybe.unwrap
                ( model, Cmd.none )
                (\code -> update (ParseFormula code) model)
                (Maybe.map .code initialFormulaM)

        initialFormula =
            Maybe.withDefault noFormula initialFormulaM
    in
    Model
        urlPrefix
        Editor
        []
        ReadOnly
        spec
        (PartialFormula initialFormula Nothing)
        (PartialFormula initialFormula Nothing)
        False
        "<noname>"
        Dict.empty
        |> doInit


editorHeight =
    A.attribute "style" "--min-height-editor: 36vh"


viewHeader : State -> Html Msg
viewHeader state =
    let
        ( newState, sign ) =
            case state of
                ReadOnly ->
                    ( Edition, "✎" )

                Edition ->
                    ( ReadOnly, "💾" )
    in
    H.header
        [ A.class "code_left" ]
        [ H.span [] [ H.text "Formula edition" ]
        , H.a [ Events.onClick (ChangeState newState) ] [ H.text sign ]
        ]


viewError : Maybe String -> List (Html Msg)
viewError =
    Maybe.map (\x -> H.span [ A.class "error" ] [ H.text x ])
        >> Maybe.toList


viewReadOnly : Model -> List (Html Msg)
viewReadOnly model =
    let
        { name, code } =
            model.current.formula
    in
    [ viewHeader model.state
    , H.div
        [ A.class "code_left" ]
        [ AceEditor.readOnly AceEditor.default code
        ]
    , H.footer [ A.class "code_left" ]
        (List.append
            [ H.button [ A.class "btn btn-primary"
                       , Events.onClick OnSave
                       ]
                  [ H.text "save as" ]
            , H.input [ A.value name, Events.onInput UpdateName ] []
            ]
            (viewError model.current.errMess)
        )
    ]


viewEdition : Model -> List (Html Msg)
viewEdition model =
    let
        cfg =
            AceEditor.default

    in
    [ viewHeader model.state
    , H.div
        [ A.class "code_left", editorHeight ]
        [ AceEditor.edit cfg model.user.formula.code model.reload
            |> H.map AceEditorMsg
        ]
    , H.footer
        [ A.class "code_left" ]
        (viewError model.user.errMess)
    , H.aside
        [ A.class "code_left center_item" ]
        [ H.text "↓" ]
    , H.header
        [ A.class "code_center" ]
        []
    , H.div
        [ A.class "code_center center_item" ]
        [ H.a
            [ Events.onClick UpdateUserFormula ]
            [ H.text "←" ]
        ]
    , H.header
        [ A.class "code_right" ]
        [ H.span [] [ H.text "Current formula" ] ]
    , H.div
        [ A.class "code_right", editorHeight ]
        [ AceEditor.readOnly cfg model.current.formula.code
        ]
    , H.footer
        [ A.class "code_right" ]
        []
    , H.aside
        [ A.class "code_right center_item" ]
        [ H.text "⬈" ]
    ]


view : Model -> Html Msg
view model =
    H.section [ A.class "code_editor" ] <|
        case model.state of
            ReadOnly ->
                viewReadOnly model

            Edition ->
                viewEdition model
