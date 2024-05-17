module Formulas exposing (main)
import Browser
import Http
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Url.Builder as UB
import Json.Decode as D
import Util as U
import Info exposing (layoutFormula)


type alias Model =
    { baseurl : String
    , formulas : List Formula
    , name : String
    , errors : List String
    , valueSearched : String
    }


type Msg =
    GotFormulas (Result Http.Error String)
    | InputChanged String


type alias Imeta =
    { formula: String
    , tzaware : Bool
    , index_type : String
    , value_type: String
    , contenthash : String
    , index_dtype : String
    , value_dtype : String
    }


type alias Formula =
    { name : String
    , imeta : Imeta
    , meta : {}
    , source : String
    , kind : String
    }


decodeImeta : D.Decoder Imeta
decodeImeta =
    D.map7 Imeta
        (D.field "formula" D.string)
        (D.field "tzaware" D.bool)
        (D.field "index_type" D.string)
        (D.field "value_type" D.string)
        (D.field "contenthash" D.string)
        (D.field "index_dtype" D.string)
        (D.field "value_dtype" D.string)


decodeFormula : D.Decoder Formula
decodeFormula =
    D.map5 Formula
        (D.field "name" D.string)
        (D.field "imeta" decodeImeta)
        (D.field "meta" (D.succeed {}))
        (D.field "source" D.string)
        (D.field "kind" D.string)


decodeListFormulas : D.Decoder (List Formula)
decodeListFormulas =
    D.list decodeFormula


view : Model -> H.Html Msg
view model =
    H.div
        [ ]
        [ H.div
            [ HA.class "action-left" ]
            [ H.div
                [  ]
                [ H.text "Search:" ]
            , H.div
                [  ]
                [ H.input
                    [ HE.onInput InputChanged
                    , HA.value model.valueSearched
                    ]
                    [ ]
                ]
            ]
        , H.div
            [ HA.class "data-table" ]
            [ H.table
                [ HA.class "table table-striped" ]
                [ H.thead [ ][ ]
                , formatTableBody model
                ]
            ]
        ]


formatTableBody : Model -> H.Html Msg
formatTableBody model =
    let

        listWords =
            (List.filter
                (\ word -> word /= "")
                (String.split " " model.valueSearched)
            )
        filtreFormulas =
            if List.isEmpty listWords then
                model.formulas
            else
                List.filter
                    (checkWordsInFormula listWords)
                    model.formulas
        formatRow : Formula -> H.Html Msg
        formatRow formula =
            H.tr
                [ ]
                [ H.td
                    [ ]
                    [ formatNamelinks model formula ]
                , H.td
                    [ ]
                    [layoutFormula model formula.imeta.formula]
                ]
    in H.tbody [ ]
        [ H.tr
            [ ]
            ( List.map formatRow filtreFormulas )
        ]


formatLink : Model -> Formula -> String -> String -> H.Html Msg
formatLink model formula page text =
    H.a
        [ HA.href <| UB.crossOrigin model.baseurl
            [ page ]
            [ UB.string "name" formula.name ]
        , HA.target "_blank"
        ]
        [H.text text]


formatNamelinks : Model -> Formula -> H.Html Msg
formatNamelinks model formula =
    H.div
        [ HA.class "action-left" ]
        [ H.div
            [  ]
            [ H.text formula.name ]
        , H.div
            [  ]
            [ formatLink model formula "tsinfo" "view" ]
        , H.div
            [  ]
            [ formatLink model formula "tsformula" "edit" ]
        ]




checkWordsInFormula : List String -> Formula -> Bool
checkWordsInFormula listWord formula =
    let
        checkWordInString : String -> Bool
        checkWordInString word =
            ( String.contains word formula.imeta.formula )
                        || (String.contains word formula.name )
    in List.all
        (\bool -> bool == True)
        (List.map checkWordInString listWord)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GotFormulas (Ok listFormulas) ->
            case D.decodeString decodeListFormulas listFormulas of
                Ok formulas ->
                    U.nocmd { model | formulas = formulas }
                Err err ->
                    doerr "getseries decode" <| D.errorToString err

        GotFormulas (Err err) ->
            doerr "getseries http" <| U.unwraperror err

        InputChanged valueSearched ->
            U.nocmd { model | valueSearched = valueSearched}


getSeriesInfo : Model -> Cmd Msg
getSeriesInfo  model =
    Http.get
        { expect = Http.expectString GotFormulas
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "find" ]
                [ UB.string "query" "(by.formula)"
                , UB.string "meta" "true"]
        }


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input Model Msg
main =
    let
        init input =
            let
                model =
                    { baseurl = input.baseurl
                    , errors = [ ]
                    , formulas = [ ]
                    , name = input.name
                    , valueSearched = ""
                    }
            in ( model , getSeriesInfo model )

    in Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
    }