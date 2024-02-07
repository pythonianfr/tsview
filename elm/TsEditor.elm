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
    , horizonModel : HorizonModel Entry
    , insertion_dates : Array String
    , name : String
    , view_nocache : Bool
    }


type Msg
    = GotEditData (Result Http.Error String)
    | HorizonSelected Horizon
    | UpdateOffset Offset


type alias Entry =
    { series : Maybe Float
    , markers : Bool
    }


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