module Addformulas exposing (main)

import Browser
import Bytes exposing (..)
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD


type Status
    = Success
    | Failure
    | Unactivated
    | CheckProcessing
    | SaveProcessing


type alias MultiStatus =
    { csv : Status
    , newcsv : Status
    , feedback : Status
    }


type alias Feedback =
    { status : String
    , errors : Dict String (List String)
    , warnings : Dict String (List String)
    , output : Dict String (List String)
    , crash : String
    }


type alias Model =
    { baseUrl : String
    , status : MultiStatus
    , csv : Maybe Bytes
    , newfile : Maybe File
    , filename : String
    , newcsv : String
    , feedback : Feedback
    }


type Msg
    = DownloadFormula
    | GotCurrentCsv (Result Http.Error Bytes)
    | GotNewCsv
    | GotNewCsvSelected File
    | GotNewCsvLoaded String
    | CheckNewCsv
    | SaveNewCsv
    | GotFeedback (Result Http.Error Feedback)


feedbackDecoder : JD.Decoder Feedback
feedbackDecoder =
    JD.map5 Feedback
        (JD.field "status" JD.string)
        (JD.field "errors" (JD.dict (JD.list JD.string)))
        (JD.field "warnings" (JD.dict (JD.list JD.string)))
        (JD.field "output" (JD.dict (JD.list JD.string)))
        (JD.field "crash" JD.string)


expectBytes : (Result Http.Error Bytes -> msg) -> Http.Expect msg
expectBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok body


downloadFormulaCsv : Model -> Cmd Msg
downloadFormulaCsv model =
    Http.get
        { url = model.baseUrl ++ "downloadformulas"
        , expect = expectBytes GotCurrentCsv
        }


downloadFile : Bytes -> Cmd Msg
downloadFile csv =
    Download.bytes "formula.csv" "text/csv" csv


loadCsv : Model -> Http.Body -> Cmd Msg
loadCsv model body =
    Http.request
        { method = "PUT"
        , headers = []
        , url =
            model.baseUrl
                ++ "updateformulas"
        , body = body
        , expect = Http.expectJson GotFeedback feedbackDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


checkNewCsv : Model -> Cmd Msg
checkNewCsv model =
    case model.newfile of
        Just file ->
            loadCsv
                model
                (Http.multipartBody
                    [ Http.filePart "new_formula.csv" file
                    ]
                )

        Nothing ->
            Cmd.none


saveNewCsv : Model -> Cmd Msg
saveNewCsv model =
    case model.newfile of
        Just file ->
            loadCsv
                model
                (Http.multipartBody
                    [ Http.filePart "new_formula.csv" file
                    , Http.stringPart "reallydoit" "Really do it"
                    ]
                )

        Nothing ->
            Cmd.none


nocmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        status =
            model.status
    in
    case msg of
        DownloadFormula ->
            ( model
            , downloadFormulaCsv model
            )

        GotCurrentCsv (Ok csv) ->
            ( { model
                | status = { status | csv = Success }
                , csv = Just csv
              }
            , downloadFile csv
            )

        GotCurrentCsv (Err _) ->
            nocmd { model | status = { status | csv = Failure } }

        GotNewCsv ->
            ( model
            , File.Select.file [ "text/csv" ] GotNewCsvSelected
            )

        GotNewCsvSelected file ->
            nocmd
                { model
                    | status =
                        { status
                            | newcsv = Success
                            , feedback = Unactivated
                        }
                    , newfile = Just file
                    , filename = File.name file
                    , feedback =
                        { status = ""
                        , errors = Dict.empty
                        , warnings = Dict.empty
                        , output = Dict.empty
                        , crash = ""
                        }
                }

        GotNewCsvLoaded content ->
            nocmd
                { model
                    | status = { status | newcsv = Success }
                    , newcsv = content
                }

        CheckNewCsv ->
            ( { model | status = { status | feedback = CheckProcessing } }
            , checkNewCsv model
            )

        SaveNewCsv ->
            ( { model | status = { status | feedback = SaveProcessing } }
            , saveNewCsv model
            )

        GotFeedback (Ok feedback) ->
            nocmd
                { model
                    | status = { status | feedback = Success }
                    , feedback = feedback
                }

        GotFeedback (Err _) ->
            nocmd { model | status = { status | feedback = Failure } }


displayWarnings : Dict String (List String) -> H.Html Msg
displayWarnings feedbackSub =
    H.ul [ HA.hidden (Dict.isEmpty feedbackSub) ]
        (List.map
            (\key -> H.div [] [ H.p [] [ H.text key ], displaySubWarnings feedbackSub key ])
            (Dict.keys feedbackSub)
        )


displaySubWarnings : Dict String (List String) -> String -> H.Html Msg
displaySubWarnings feedbackSub key =
    H.ul
        []
        (List.map (\sn -> H.li [] [ H.text sn ]) (Maybe.withDefault [] (Dict.get key feedbackSub)))


view : Model -> H.Html Msg
view model =
        H.div
            []
            [ H.h1 [ HA.class "header-refinery"] [ H.text "Load a formula batch" ]
            , H.div [ HA.class "addformulas-content" ]
                [ H.div
                    [ HA.class "addformulas-card" ]
                    [ H.h4 [ HA.class "addformulas-card-title" ] [ H.text "How to ?" ]
                    , H.br [] []
                    , H.p
                        [ HA.class "addformulas-card-text" ]
                        [ H.text
                            "Follow these steps : "
                        , H.br [] []
                        , H.ul []
                            [ H.p
                                []
                                [ H.text "1. Download the current formulas csv" ]
                            , H.p
                                []
                                [ H.text "2. Modify it (note: removing a line will not result in deleting a formula)" ]
                            , H.p
                                []
                                [ H.text "3. Load the modified file" ]
                            , H.p
                                []
                                [ H.text "4. Check that your file is valid" ]
                            , H.p
                                []
                                [ H.text "5. Read carefully the Errors & Warnings section below" ]
                            , H.p
                                []
                                [ H.text "6. When it looks good, click on «Register new formulas»" ]
                            ]
                        ]
                    ]
                , H.br [] []
                , H.div [ HA.class "flex-container" ]
                    [ H.button
                        [ HA.class "addformulas-button"
                        , HA.class "greenbutton"
                        , HE.onClick DownloadFormula
                        , HA.title "Download current state of formulas"
                        ]
                        [ H.text "↓ Download current csv" ]
                    , H.p [ HA.hidden (model.status.csv /= Success), HA.class "greentext" ] [ H.text "✓ Done" ]
                    ]
                , H.hr [] []
                , H.div [ HA.class "flex-container" ]
                    [ H.button
                        [ HA.class "addformulas-button"
                        , HA.class "apibutton"
                        , HE.onClick GotNewCsv
                        ]
                        [ H.text "🔎 Load formula CSV" ]
                    , H.p
                        [ HA.hidden (model.status.newcsv /= Success)
                        , HA.class "greentext"
                        ]
                        [ H.text ("✓ Loaded : " ++ model.filename) ]
                    ]
                , H.br [] []
                , H.div [ HA.class "flex-container" ]
                    [ H.button
                        [ HA.class "addformulas-button"
                        , HA.class "orangebutton"
                        , HE.onClick CheckNewCsv
                        , HA.hidden (model.filename == "")
                        ]
                        [ H.text "Check CSV" ]
                    , H.img
                        [ HA.hidden (model.status.feedback /= CheckProcessing)
                        , HA.src "./tsview_static/loading_wheel.gif"
                        , HA.class "img-loading"
                        ]
                        []
                    , H.p
                        [ HA.hidden (not (model.feedback.status == "valid"))
                        , HA.class "greentext"
                        ]
                        [ H.text "✓ All green" ]
                    , H.p
                        [ HA.hidden (not (model.feedback.status == "invalid"))
                        , HA.class "redtext"
                        ]
                        [ H.text "Errors have been raised. Please correct your file." ]
                    , H.p
                        [ HA.hidden (model.status.feedback /= Failure)
                        , HA.class "redtext"
                        ]
                        [ H.text "Fail to check file" ]
                    ]
                , H.div
                    [ HA.class "warnings"
                    , HA.hidden (model.filename == "" || Dict.isEmpty model.feedback.warnings)
                    ]
                    [ H.u [ HA.hidden (Dict.isEmpty model.feedback.warnings) ] [ H.text "Warnings : " ]
                    , displayWarnings model.feedback.warnings
                    , H.br [] []
                    ]
                , H.div
                    [ HA.class "errors"
                    , HA.hidden (model.filename == "" || Dict.isEmpty model.feedback.errors)
                    ]
                    [ H.u [ HA.hidden (Dict.isEmpty model.feedback.errors) ] [ H.text "Errors : " ]
                    , displayWarnings model.feedback.errors
                    ]
                , H.br [] []
                , H.div [ HA.class "flex-container" ]
                    [ H.button
                        [ HA.class "addformulas-button"
                        , HA.class "redbutton"
                        , HE.onClick SaveNewCsv
                        , HA.hidden
                            (not
                                ((model.filename /= "")
                                    && Dict.isEmpty model.feedback.errors
                                    && ((model.status.feedback == Success) || model.status.feedback == SaveProcessing)
                                )
                            )
                        ]
                        [ H.text "Register new formulas" ]
                    , H.p
                        [ HA.hidden (not (model.feedback.status == "saved"))
                        , HA.class "greentext"
                        ]
                        [ H.text "✓ Done" ]
                    , H.p
                        [ HA.hidden (not (model.feedback.status == "fail"))
                        , HA.class "redtext"
                        ]
                        [ H.text "x Failed" ]
                    , H.img
                        [ HA.hidden (model.status.feedback /= SaveProcessing)
                        , HA.src "./tsview_static/loading_wheel.gif"
                        , HA.class "img-loading"
                        ]
                        []
                    ]
                , H.div
                    [ HA.class "errors"
                    , HA.hidden (model.filename == "" || (model.feedback.crash == ""))
                    ]
                    [ H.u [ HA.hidden (model.feedback.crash == "") ] [ H.text "Crash : " ]
                    , H.br [] []
                    , H.p [ HA.hidden (model.feedback.crash == "") ] [ H.text model.feedback.crash ]
                    ]
                , H.div
                    [ HA.class "allgreen"
                    , HA.hidden (model.filename == "" || Dict.isEmpty model.feedback.output)
                    ]
                    [ H.u [ HA.hidden (Dict.isEmpty model.feedback.output) ] [ H.text "Output : " ]
                    , displayWarnings model.feedback.output
                    ]
                , H.br [] []
                ]
            ]



subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.none


initModel baseurl =
    { baseUrl = baseurl
    , status =
        { csv = Unactivated
        , newcsv = Unactivated
        , feedback = Unactivated
        }
    , csv = Nothing
    , newfile = Nothing
    , filename = ""
    , newcsv = ""
    , feedback =
        { status = ""
        , errors = Dict.empty
        , warnings = Dict.empty
        , output = Dict.empty
        , crash = ""
        }
    }


type alias Input =
    { baseurl : String }


init : Input -> ( Model, Cmd Msg )
init input =
    ( initModel input.baseurl
    , Cmd.none
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
