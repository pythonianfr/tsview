module TsEditor exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Either exposing (Either(..))
import Horizon exposing
    ( Horizon
    , HorizonModel
    , Offset
    , defaultHorizon
    , horizonbtnGroup
    , horizons
    , updateHorizonModel
    )
import Http
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Maybe.Extra as Maybe
import Metadata as M
import Plotter exposing
    ( getData
    , scatterplot
    , plotargs
    )
import Url.Builder as UB
import Util as U
import Json.Encode as E


type alias Model =
    { baseurl : String
    , editedDataError : Bool
    , errors : List String
    , meta : M.StdMetadata
    , date_index : Int
    , editedtimeSeries : EditedData
    , horizonModel : HorizonModel Entry
    , insertion_dates : Array String
    , name : String
    , view_nocache : Bool
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotMetadata (Result Http.Error String)
    | HorizonSelected Horizon
    | UpdateOffset Offset
    | InputChanged String String
    | SaveEditedData
    | GotEditedData (Result Http.Error String)


type alias Entry =
    { series : Maybe Float
    , markers : Bool
    , processingEdition : Bool
    }


type alias EditedData =
   Dict String String


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map3 Entry
        (D.field "series" (D.maybe D.float))
        (D.field "markers" D.bool)
        (D.succeed False)


dataDecoder : D.Decoder (Dict String Entry)
dataDecoder =
    D.dict entryDecoder


geteditor : Model -> Bool -> Cmd Msg
geteditor model atidate =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getData
            { baseurl = model.baseurl
            , name = model.name
            , idate = (if atidate then idate else Nothing)
            , callback = GotEditData
            , nocache = (U.bool2int model.view_nocache)
            , fromdate = Maybe.unwrap
                "" (always model.horizonModel.mindate) model.horizonModel.horizon.key
            , todate = Maybe.unwrap
                "" (always model.horizonModel.maxdate) model.horizonModel.horizon.key
            , horizon = model.horizonModel.horizon.key
                |> Maybe.andThen (\key-> Dict.get key horizons)
                |> Maybe.map
                    (String.replace "{offset}" (String.fromInt model.horizonModel.offset))
           }
           "supervision"
           "true"


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of

        GotEditData (Ok rawdata) ->
            case D.decodeString dataDecoder rawdata of
                Ok val ->
                    U.nocmd { model | horizonModel =
                                updateHorizonModel model.horizonModel val
                            }
                Err _ ->
                  U.nocmd model

        GotEditData (Err _) ->
            U.nocmd model

        HorizonSelected horizon ->
            updateHorizon horizon 0 model

        UpdateOffset (Left i) ->
            updateHorizon model.horizonModel.horizon (model.horizonModel.offset + i) model

        UpdateOffset (Right i) ->
            updateHorizon model.horizonModel.horizon (model.horizonModel.offset - i) model

        InputChanged date value ->
            let
                newModel =
                    if checkSeries date value model.horizonModel.timeSeries then
                      updateProcessingModel
                        model
                        False
                        date
                        (Dict.remove date model.editedtimeSeries)
                    else
                 updateProcessingModel
                        model
                        True
                        date
                        (Dict.insert date value model.editedtimeSeries)

            in
            U.nocmd newModel

        SaveEditedData ->
            (model, patchEditedData model)

        GotEditedData (Ok _) ->
            ({ model | editedtimeSeries = Dict.empty }, geteditor model False)

        GotEditedData (Err _) ->
            U.nocmd { model | editedDataError = True }

        GotMetadata (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    U.nocmd { model | meta = allmeta }
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotMetadata (Err err) ->
            doerr "gotmeta http" <| U.unwraperror err


updateProcessingModel : Model -> Bool -> String -> EditedData -> Model
updateProcessingModel model editionProcessing date editedData =
    let
        horizonModel = model.horizonModel
        timeSeries = horizonModel.timeSeries
        updatedTimeSeries = Dict.update
            date (updateProcessingEdition editionProcessing) timeSeries
        updatedHorizonModel = { horizonModel | timeSeries = updatedTimeSeries }
    in
    { model | editedtimeSeries = editedData
    , horizonModel = updatedHorizonModel}



updateProcessingEdition : Bool -> Maybe Entry -> Maybe Entry
updateProcessingEdition newProcessingValue maybeEntry =
    Maybe.map (\entry ->
        { entry | processingEdition = newProcessingValue }) maybeEntry


checkSeries : String -> String -> Dict String Entry -> Bool
checkSeries date value data =
    case Dict.get date data of
        Just { series } ->
            if series == String.toFloat value then
                True
            else
                False
        Nothing ->
            True


updateData : EditedData -> Dict String Entry -> Dict String Entry
updateData editedData data =
    Dict.foldl
        (\key editedDataValue accData ->
            case Dict.get key accData of
                Just dataValue ->
                    Dict.insert key { dataValue
                        | series = String.toFloat editedDataValue
                    } accData

                Nothing ->
                    accData
        )
        data
        editedData


updateHorizon : Horizon -> Int -> Model -> ( Model, Cmd Msg )
updateHorizon horizon newOffset model =
    let
        newHorizonModel = model.horizonModel
        newmodel = { model
                        | horizonModel =
                            { newHorizonModel
                                | horizon = horizon
                                , offset = newOffset
                            }
                   }
    in
    (newmodel, geteditor newmodel False)


patchEditedData : Model -> Cmd Msg
patchEditedData model =
    let
        mtzaware =
            Maybe.withDefault (M.MBool True) <|
                Dict.get "tzaware" model.meta

        tzaware =
            case mtzaware of
                M.MBool b -> b
                _ -> True
    in
    Http.request
        { method = "PATCH"
        , body = Http.jsonBody <| E.object
                 [ ("name", E.string model.name )
                 , ("author" , E.string "webui" )
                 , ("tzaware", E.bool tzaware )
                 , ("series", encodeEditedData model.editedtimeSeries )
                 , ("supervision", E.bool True )
                 ]
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "state" ] [ ]
        , expect = Http.expectString GotEditedData
        }


encodeEditedData : EditedData -> E.Value
encodeEditedData editedData =
    E.dict
        identity
        (\value ->
            if value == "" then
                E.null
            else
                E.float (Maybe.withDefault 0.0 (String.toFloat value))
        )
        editedData


viewEditedRow : Model -> H.Html Msg
viewEditedRow model =
    let
        row : (String, String) -> H.Html Msg
        row (date, value) =
            H.tr
                [ ]
                [ H.td [ ] [ H.text (String.slice 0 19 date) ]
                , H.td [ ] [ H.text value]
                ]
    in
    if Dict.isEmpty model.editedtimeSeries then
        H.div [ ][ ]
    else
        H.div [ HA.class "col-sm" ]
        [ H.button
            [ HA.class "btn btn-warning"
            , HA.attribute "type" "button"
            , HE.onClick SaveEditedData ]
            [ H.text "Save" ]
        , if model.editedDataError then
            H.text "Server error: please try again "
        else
            H.text ""
        , H.table
            [ HA.class "table-sm table-bordered custom-table table-striped" ]
            [ H.thead
                [ ]
                [ H.tr
                    [ ]
                    [ H.th
                        [ HA.scope "col" ]
                        [ H.text "Dates" ]
                    , H.th
                        [ HA.scope "col" ]
                        [ H.text "Value" ]
                    ]
                ]
            , H.tbody
                [ HA.class "text-warning" ]
                (List.map row (Dict.toList model.editedtimeSeries))
            ]
        ]


viewRow : (String, Entry) -> H.Html Msg
viewRow ( date, data ) =
    let
        rowStyle =
            if data.processingEdition then
                [HA.class "text-warning"]
            else if data.markers then
                [HA.class "text-success"]
            else if Maybe.isNothing data.series then
                [HA.class "text-danger"]
            else
                []
        initialValue = Maybe.withDefault "" (Maybe.map String.fromFloat data.series)
        propagedMessage : String -> D.Decoder Msg
        propagedMessage value =
            if value == initialValue then
                D.fail "no propagation"
            else
                D.succeed (InputChanged date value)
    in
    H.tr rowStyle
        [ H.td
            [ ]
            [ H.text (String.slice 0 19 date)
            ]
        , H.td
            [ ]
            [ H.input
                [ HA.placeholder "enter your value"
                , HA.type_ "number"
                , HA.value initialValue
                , HE.on "input" (D.andThen propagedMessage HE.targetValue)
                ]
                [ ]
            ]
        ]


viewPlotData : Model -> H.Html Msg
viewPlotData model =
    let
        plot = scatterplot model.name
            (Dict.keys model.horizonModel.timeSeries)
            (List.map (\x -> x.series) (Dict.values model.horizonModel.timeSeries))
            "lines"
        args = plotargs "plot" [plot]
    in H.div
        [ ]
        [ H.div [ HA.id "plot" ] [ ]
        , H.node "plot-figure" [ HA.attribute "args" args ] []
        ]


view : Model -> H.Html Msg
view model =
    H.div
        [ ]
        [ horizonbtnGroup model.horizonModel UpdateOffset HorizonSelected
        , viewPlotData model
        , H.div [ HA.class "container" ]
            [ H.div
                [ HA.class "row" ]
                [ editTable model
                , viewEditedRow model
                ]
            ]
        ]


editTable : Model -> H.Html Msg
editTable model =
    if Dict.isEmpty model.horizonModel.timeSeries
    then H.div [ ][ ]
    else
        H.div
        [ HA.class "col-sm" ]
        [ H.table
            [ HA.class
                "table-sm table-bordered custom-table table-striped"
            ]
            [ H.thead [ ]
                [ H.tr [ ]
                    [ H.th
                        [ HA.scope "col" ]
                        [ H.text "Dates" ]
                    , H.th
                        [ HA.scope "col" ]
                        [ H.text "Value" ]
                    ]
                ]
            , H.tbody [ ]
                (List.map
                    viewRow
                    (Dict.toList
                        (updateData
                            model.editedtimeSeries
                            model.horizonModel.timeSeries
                        )
                    )
                )
            ]
        ]


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
                        , editedDataError = False
                        , errors = []
                        , meta = Dict.empty
                        , date_index = 0
                        , editedtimeSeries = Dict.empty
                        ,  horizonModel =
                            { offset = 0
                            , offset_reached = False
                            , horizon = {key = Just defaultHorizon}
                            , mindate = ""
                            , maxdate = ""
                            , timeSeries = Dict.empty
                            }
                        , insertion_dates = Array.empty
                        , name = input.name
                        , view_nocache = False
                        }
               in
               ( model
               , Cmd.batch
                   [ (geteditor model False)
                   , M.getsysmetadata model.baseurl model.name GotMetadata "series"
                   ]
               )

       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = \model -> Sub.none
               }



