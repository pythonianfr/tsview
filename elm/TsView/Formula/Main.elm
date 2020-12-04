module TsView.Formula.Main exposing (main)

import Browser
import Dict exposing (Dict)
import Either
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode exposing (Value)
import List.Nonempty as NE exposing (Nonempty)
import Plotter exposing
    ( scatterplot
    , plotargs
    )
import TsView.Formula.CodeEditor as CodeEditor
import TsView.Formula.EditionTree.Main as EditionTree
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.Spec.Parser exposing (parseSpecValue)
import Util as U


type Msg
    = CodeEditorMsg CodeEditor.Msg
    | EditionTreeMsg EditionTree.Msg
    | SwitchTab Tab


type Tab
    = Editor
    | Plot


type alias Model =
    { urlPrefix : String
    , errors : List String
    , tab : Tab
    , codeEditor : CodeEditor.Model
    , editionTree : EditionTree.Model
    , specerrors : Maybe (Nonempty String)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr error =
            U.nocmd <| U.adderror model error
    in
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

        SwitchTab tab ->
            case tab of
                Editor -> U.nocmd { model | tab = Plot }
                Plot -> U.nocmd { model | tab = Editor }


viewtabs model =
    H.ul
        [ A.id "tabs"
        , A.class "nav nav-tabs"
        , A.attribute "role" "tablist"
        ]
        [ H.li [ A.class "nav-item" ]
          [ H.a [ A.class <| "nav-link" ++ (if model.tab == Editor then " active" else "")
                , A.attribute "data-toggle" "tab"
                , A.attribute "role" "tab"
                , E.onClick (SwitchTab Plot)
                ] [ H.text "Editor" ]
          ]
        , H.li [ A.class "nav-item" ]
          [ H.a [ A.class <| "nav-link" ++ (if model.tab == Plot then " active" else "")
                , A.attribute "data-toggle" "tab"
                , A.attribute "role" "tab"
                , E.onClick (SwitchTab Editor)
                ] [ H.text "Plot" ]
          ]
        ]


viewplot cemodel =
    let
        plot =
            scatterplot cemodel.name
            (Dict.keys cemodel.plotdata)
            (Dict.values cemodel.plotdata)
            "lines"
        args = plotargs "plot" [plot]
    in
    -- the "plot-figure" node is pre-built in the template side
    -- (html component)
    H.node "plot-figure" [ A.attribute "args" args ] []


view : Model -> Html Msg
view model =
    H.div [ A.style "margin" ".5em" ]
        [ viewtabs model
        -- the plot div hates being set too dynamically so
        -- we put it on the toplevel and show/hide it depending
        -- on the tab
        , case model.specerrors of
              Nothing -> H.span [] []
              Just errlist ->
                  H.div [] <| List.map (\x -> H.span [] [ H.text x ]) <| NE.toList errlist
        , case model.tab of
              Editor -> H.div [ A.id "plot", A.style "display" "none" ] []
              Plot -> H.div [ A.id "plot" ] []
        , case model.tab of
              Editor ->
                  H.article [ A.class "main" ]
                      [ H.map CodeEditorMsg (CodeEditor.view model.codeEditor)
                      , H.map EditionTreeMsg (EditionTree.view model.editionTree)
                      ]

              Plot ->
                  viewplot model.codeEditor
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
                []
                Editor
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
