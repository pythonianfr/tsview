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
import Plotter exposing
    ( getData
    , scatterplot
    , plotargs
    )
import Util as U


type alias Model =
    { baseurl : String
    , date_index : Int
    , editedtimeSeries : EditedData
    , horizonModel : HorizonModel Entry
    , insertion_dates : Array String
    , name : String
    , view_nocache : Bool
    }


type Msg
    = GotEditData (Result Http.Error String)
    | HorizonSelected Horizon
    | UpdateOffset Offset
    | InputChanged String String


type alias Entry =
    { series : Maybe Float
    , markers : Bool
    }


type alias EditedData =
   Dict String String


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map2 Entry
        (D.field "series" (D.maybe D.float))
        (D.field "markers" D.bool)


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
    case msg of

        GotEditData (Ok rawdata) ->
            case D.decodeString dataDecoder rawdata of
                Ok val ->
                 U.nocmd {
                        model | horizonModel =
                            updateHorizonModel model.horizonModel val}
                Err _ ->
                  (model, Cmd.none)

        GotEditData (Err _) ->
                  (model, Cmd.none)

        HorizonSelected horizon ->
            updateHorizon horizon 0 model

        UpdateOffset (Left i) ->
            updateHorizon model.horizonModel.horizon (model.horizonModel.offset + i) model

        UpdateOffset (Right i) ->
            updateHorizon model.horizonModel.horizon (model.horizonModel.offset - i) model

        InputChanged date value ->
            let
                newDict =
                    if checkSeries date value model.horizonModel.timeSeries then
                        Dict.remove date model.editedtimeSeries
                    else
                        Dict.insert date value model.editedtimeSeries
            in
            ( { model
                | editedtimeSeries = newDict}
            , Cmd.none )


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
                        | series = String.toFloat editedDataValue } accData

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
                                , offset = newOffset}}
    in
    (newmodel, geteditor newmodel False)


viewRow : (String, Entry) -> H.Html Msg
viewRow ( date, data ) =
    let
        rowStyle =
            if data.markers then
                [HA.class "table-danger"]
            else
                []
        initialValue =  Maybe.withDefault "" (Maybe.map String.fromFloat data.series)
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
                [ HA.placeholder "Enter your value"
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
                , viewEditedRow model.editedtimeSeries
                ]
            ]
        ]


viewEditedRow : EditedData -> H.Html Msg
viewEditedRow editedData =
    let
        row : (String, String) -> H.Html Msg
        row (date, value) =
            H.tr
                [ ]
                [ H.td [ ] [ H.text (String.slice 0 19 date) ]
                , H.td [ ] [ H.text value]
                ]
    in
    if Dict.isEmpty editedData then
            H.div [ ][ ]
    else
        H.div [ HA.class "col-sm" ]
        [ H.table
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
                [ ]
                (List.map row (Dict.toList editedData))
            ]
        ]


editTable : Model -> H.Html Msg
editTable model =
    if Dict.isEmpty model.horizonModel.timeSeries then
        H.div [ ][ ]
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
               ( model, (geteditor model False) )

       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = \model -> Sub.none
               }