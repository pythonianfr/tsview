module TsEditor exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Either exposing (Either(..))
import Horizon exposing
    ( HorizonModel
    , defaultHorizon
    , horizons
    )
import Http
import Html as H
import Html.Attributes as HA
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


type Msg =
    GotEditData (Result Http.Error String)


type alias Entry =
    { series : Float
    , markers : Bool
    }

entryDecoder : D.Decoder Entry
entryDecoder =
    D.map2 Entry
        (D.field "series" D.float)
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
                    let
                        newHorizonModel = model.horizonModel
                    in  ( { model
                            | horizonModel =
                                { newHorizonModel | timeSeries = val }
                            }
                        , Cmd.none )
                Err _ ->
                    (model, Cmd.none)

        GotEditData (Err _) ->
                    (model, Cmd.none)


viewPlotData : Model -> H.Html Msg
viewPlotData model =
    let
        plot = scatterplot model.name
            (Dict.keys model.horizonModel.timeSeries)
            (List.map (\x -> x.series) (Dict.values  model.horizonModel.timeSeries))
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
        [ viewPlotData model
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