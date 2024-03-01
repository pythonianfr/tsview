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
import Info as I
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
import Random


type alias Model =
    { baseurl : String
    , contentCell: String
    , editedDataError : Bool
    , errors : List String
    , meta : M.StdMetadata
    , date_index : Int
    , horizonModel : HorizonModel Entry
    , indexToInsert: Maybe String
    , insertion_dates : Array String
    , name : String
    , processedPasted: List String
    , rawPasted: String
    , view_nocache : Bool
    , randomNumber : Int
    }


type Msg
    = GotEditData (Result Http.Error String)
    | GotMetadata (Result Http.Error String)
    | HorizonSelected Horizon
    | UpdateOffset Offset
    | InputChanged String String
    | SaveEditedData
    | GotEditedData (Result Http.Error String)
    | Paste PasteType
    | InsertionDates (Result Http.Error String)
    | GetLastInsertionDates (Result Http.Error String)
    | GetLastEditedData (Result Http.Error String)
    | RandomNumber Int


type alias Entry =
    { series : Maybe Float
    , markers : Bool
    , editedValue : Maybe String
    , index : Int
    }


type alias PasteType =
    { text: String
    , index: String}


textDecoder: D.Decoder String
textDecoder =
     D.at [ "detail", "text" ] D.string


indexDecoder: D.Decoder String
indexDecoder =
     D.at [ "detail", "index" ] D.string


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map4 Entry
        (D.field "series" (D.maybe D.float))
        (D.field "markers" D.bool)
        (D.succeed Nothing)
        (D.succeed 0)


pasteWithDataDecoder : D.Decoder PasteType
pasteWithDataDecoder =
        D.map2 PasteType textDecoder indexDecoder


dataDecoder : D.Decoder (Dict String Entry)
dataDecoder =
    D.dict entryDecoder


onPaste : (PasteType -> msg) -> H.Attribute msg
onPaste msg =
  HE.on "pastewithdata" (D.map msg pasteWithDataDecoder)


process : String -> List String
process raw =

    if String.contains "\n" raw
    then
        String.split "\n" raw
    else
    if String.contains "\r" raw
    then
        String.split "\r" raw
    else
    if String.contains " " raw
    then
        String.split " " raw
    else
        [raw]


geteditor : Model -> Bool -> (Result Http.Error String -> Msg) -> Cmd Msg
geteditor model atidate msg =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getData
            { baseurl = model.baseurl
            , name = model.name
            , idate = (if atidate then idate else Nothing)
            , callback = msg
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


incrementIndex : Int -> (String, Entry) -> (String, Entry)
incrementIndex increment tupleDateData =
    let
        data = Tuple.second tupleDateData
    in
    (Tuple.first tupleDateData, { data | index = increment })


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
                    let
                        incrementedVal = Dict.fromList
                            (List.indexedMap
                                incrementIndex
                                (Dict.toList val)
                            )
                    in
                    U.nocmd { model | horizonModel =
                                updateHorizonModel
                                    model.horizonModel incrementedVal
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
                newtimeSeries = Dict.update
                    date (updateEntry (parseCopyPastedData value)) model.horizonModel.timeSeries
                newHorizonModel = model.horizonModel
            in
            U.nocmd { model
                | horizonModel = { newHorizonModel | timeSeries = newtimeSeries }
                }

        GetLastInsertionDates (Ok rawdates) ->
            case D.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    let
                        currentInsertionDate =
                            Array.get model.date_index model.insertion_dates
                        lastInsertionDate = Array.get
                            (List.length dates - 1)
                            (Array.fromList dates)
                    in
                    if currentInsertionDate /= lastInsertionDate then
                        ( model, (geteditor model False GetLastEditedData) )
                    else
                        (model, patchEditedData model)

                Err err ->
                    doerr "idates decode" <| D.errorToString err

        GetLastInsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        GetLastEditedData (Ok rawdata) ->
            case D.decodeString dataDecoder rawdata of
                Ok val ->
                    let
                        incrementedVal = Dict.fromList
                            (List.indexedMap
                                incrementIndex
                                (Dict.toList val)
                            )

                        editEntry : Entry -> Maybe Entry -> Maybe Entry
                        editEntry value maybeEntry =
                            Maybe.andThen
                                (\entry ->
                                    Just { entry
                                        | editedValue = value.editedValue })
                                maybeEntry

                        newDict =
                            Dict.merge
                                (\_ _ dict -> dict)
                                (\key _ value dict ->
                                    Dict.update key (editEntry value) dict)
                                (\_ _ dict -> dict)
                                incrementedVal
                                model.horizonModel.timeSeries
                                incrementedVal

                        newModel = { model | horizonModel =
                                updateHorizonModel
                                    model.horizonModel newDict
                            }
                    in
                    (newModel, patchEditedData newModel)

                Err _ ->
                  U.nocmd model

        GetLastEditedData (Err _) ->
            U.nocmd model

        SaveEditedData ->
            ( model, I.getidates model "series" GetLastInsertionDates)

        GotEditedData (Ok _) ->
            (model, Cmd.batch [
                    geteditor model False GotEditData,
                    Random.generate RandomNumber randomInt]
            )

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

        Paste payload ->
            let
                newtimeSeries = getPastedDict model payload
                newHorizonModel = model.horizonModel
            in
            U.nocmd { model
                | horizonModel = { newHorizonModel | timeSeries = newtimeSeries }
                }

        InsertionDates (Ok rawdates) ->
            case D.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    U.nocmd { model
                                | insertion_dates = Array.fromList dates
                                , date_index = List.length dates - 1
                            }
                Err err ->
                    doerr "idates decode" <| D.errorToString err

        InsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        RandomNumber number ->
            ({ model | randomNumber = number }, Cmd.none)


randomInt : Random.Generator Int
randomInt =
  Random.int 0 999999999


parseCopyPastedData : String -> Maybe String
parseCopyPastedData value =
    case String.toFloat (String.replace "," "." value) of
        Just _ ->
            Just (String.replace "," "." value)

        Nothing ->
            if value == "" then
                Just ""
            else
                Nothing


getPastedDict : Model -> PasteType -> Dict String Entry
getPastedDict model payload =
    let
        newValues = List.map parseCopyPastedData (process payload.text)
        firstIndex = Maybe.unwrap
            0
            (\entry -> entry.index)
            (Dict.get (payload.index) model.horizonModel.timeSeries)
        listIndex = List.range
                        firstIndex (firstIndex + (List.length newValues) - 1)
        listDates = Dict.keys
                        (Dict.filter
                        (\_ value -> List.member value.index listIndex)
                        model.horizonModel.timeSeries)
        copyPastedDict = Dict.fromList
            (List.map2
                Tuple.pair listDates newValues)
    in Dict.merge
        (\_ _ dict -> dict)
        (\key _ value dict -> Dict.update key (updateEntry value) dict)
        (\_ _ dict -> dict)
        model.horizonModel.timeSeries
        copyPastedDict
        model.horizonModel.timeSeries


updateEntry : Maybe String -> Maybe Entry -> Maybe Entry
updateEntry value maybeEntry =
    maybeEntry
        |> Maybe.andThen (\entry ->
            if (parseCopyPastedData (
                (Maybe.unwrap "" String.fromFloat entry.series))) /= value then
                Just { entry | editedValue = value }
            else
                Just { entry | editedValue = Nothing }
        )


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
    (newmodel, Cmd.batch [
         geteditor newmodel False GotEditData,
        Random.generate RandomNumber randomInt])


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

        filteredDict =
            model.horizonModel.timeSeries
                |> Dict.filter (\_ value -> Maybe.isJust value.editedValue)
                |> Dict.map (\_ value -> Maybe.withDefault "" value.editedValue)
    in
    Http.request
        { method = "PATCH"
        , body = Http.jsonBody <| E.object
                 [ ("name", E.string model.name )
                 , ("author" , E.string "webui" )
                 , ("tzaware", E.bool tzaware )
                 , ("series", encodeEditedData filteredDict )
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


encodeEditedData : Dict String String -> E.Value
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
        row : (String, Entry) -> H.Html Msg
        row (date, entry) =
            H.tr
                [ ]
                [ H.td [ ] [ H.text (String.slice 0 19 date) ]
                , H.td [ ] [ H.text (Maybe.withDefault "" entry.editedValue)]
                ]
        filtredDict = Dict.filter
            (\_ entry -> Maybe.isJust entry.editedValue)
            model.horizonModel.timeSeries
    in
    if Dict.isEmpty filtredDict then
        H.div [ HA.class "col-sm" ][ ]
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
                (List.map row (Dict.toList filtredDict))
            ]
        ]


viewRow : (String, Entry) -> H.Html Msg
viewRow ( date, entry ) =
    let
        data =
            if Maybe.isJust entry.editedValue then
                Maybe.withDefault "" entry.editedValue
            else
                 Maybe.unwrap "" String.fromFloat entry.series

        rowStyle =
            if Maybe.isJust entry.editedValue then
                [HA.class "text-warning"]
            else if entry.markers == True then
                [HA.class "text-success"]
            else if Maybe.isNothing entry.series then
                [HA.class "text-danger"]
            else
                [ ]
    in
    H.tr rowStyle
        [ H.td
            [ ]
            [ H.text (String.slice 0 19 date)
            ]
        , H.td
            [ ]
            [ H.input
                [ HA.class "pasteble"
                , HA.placeholder "enter your value"
                , HA.value data
                , HE.onInput (InputChanged date)
                , HA.attribute "index" date
                , onPaste Paste
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
                    (Dict.toList model.horizonModel.timeSeries)
                )
            ]
        , H.node "eval-js"
            [ HA.attribute
                "myjs"
                ("applyCopyPaste(" ++ String.fromInt model.randomNumber ++ ");") ]
            [ ]
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
                        , contentCell = "68"
                        , editedDataError = False
                        , errors = []
                        , meta = Dict.empty
                        , date_index = 0
                        ,  horizonModel =
                            { offset = 0
                            , offset_reached = False
                            , horizon = {key = Just defaultHorizon}
                            , mindate = ""
                            , maxdate = ""
                            , timeSeries = Dict.empty
                            }
                        , indexToInsert = Nothing
                        , insertion_dates = Array.empty
                        , name = input.name
                        , processedPasted = []
                        , randomNumber = 0
                        , rawPasted = ""
                        , view_nocache = False
                        }
               in
               ( model
               , Cmd.batch
                   [ (geteditor model False GotEditData)
                   , M.getsysmetadata model.baseurl model.name GotMetadata "series"
                   , I.getidates model "series" InsertionDates
                   ]
               )

       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = \model -> Sub.none
               }