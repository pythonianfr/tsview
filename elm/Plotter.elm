module Plotter exposing
    ( defaultoptions
    , defaultLayoutOptions
    , defaultConfigOptions
    , getdata
    , getgroupplotdata
    , Group
    , groupdecoder
    , scatterplot
    , serializedPlotArgs
    , seriesdecoder
    , Series
    )

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import List.Extra as List
import Maybe.Extra as Maybe
import Url.Builder as UB
import Bool.Extra


type alias LayoutOptions =
    { title: Maybe String
    , dragMode: Maybe String
    , xaxis: Maybe { range: List String }
    , yaxis: Maybe { range: List Float }
    , height: Maybe Int
    }


defaultLayoutOptions: LayoutOptions
defaultLayoutOptions =
    { title = Nothing
    , dragMode = Nothing
    , xaxis = Nothing
    , yaxis = Nothing
    , height = Nothing
    }


type alias ConfiOptions =
    { displaylogo: Bool
    , displayModeBar: Bool
    , modeBarButtonsToRemove: List String
    , responsive: Bool
    , showlegend: Bool}


defaultConfigOptions: ConfiOptions
defaultConfigOptions =
    { displaylogo = False
    , displayModeBar = True
    , modeBarButtonsToRemove = [ "sendDataToCloud" ]
    , responsive= True
    , showlegend = False
    }

type alias TraceOptions =
    { showlegend: Bool
    , line: Maybe { color: String }
    , opacity: Float
    }


defaultoptions =
    { showlegend = False
    , line = Nothing
    , opacity = 1
    }


type alias Trace =
    { type_ : String
    , name : String
    , x : List String
    , y : List (Maybe Float)
    , mode : String
    , options : TraceOptions
    }


type alias CallBack msg =
    (Result Http.Error String -> msg)


type alias PlotQuery msg =
    { baseurl : String
    , apipoint : String
    , name : String
    , idate : Maybe String
    , callback : CallBack msg
    , nocache : Int
    , fromdate : String
    , todate : String
    , horizon : Maybe String
    , inferredFreq : Bool
    , tzone : String
    , keepnans : Bool
    }


encodetrace : Trace -> E.Value
encodetrace t =
    case t.options.line of
        Nothing ->
            E.object
                [ ( "type", E.string t.type_ )
                , ( "name", E.string t.name )
                , ( "x", E.list E.string t.x )
                , ( "y", E.list (E.maybe E.float) t.y )
                , ( "mode", E.string t.mode )
                , ( "showlegend",  E.bool t.options.showlegend )
                , ( "opacity", E.float t.options.opacity )
                ]
        Just line ->
            E.object
                [ ( "type", E.string t.type_ )
                , ( "name", E.string t.name )
                , ( "x", E.list E.string t.x )
                , ( "y", E.list (E.maybe E.float) t.y )
                , ( "mode", E.string t.mode )
                , ( "showlegend",  E.bool t.options.showlegend )
                , ( "line", (E.object [("color", E.string line.color)]))
                , ( "opacity", E.float t.options.opacity )
                ]


type alias Series =
    Dict String (Maybe Float)


seriesdecoder =
    D.dict (D.maybe D.float)


type alias TraceArgs =
    String -> List String -> List (Maybe Float) -> String -> TraceOptions -> Trace


scatterplot : TraceArgs
scatterplot =
    Trace "scatter"


encodeLayout : LayoutOptions -> E.Value
encodeLayout layoutOptions =
    (E.object ( List.concat [
            case layoutOptions.title of
                Nothing -> []
                Just title -> [( "title", E.string title )]

            , case layoutOptions.xaxis of
                Nothing -> []
                Just range -> [ ("xaxis", E.object [ ("range"
                              , E.list E.string [ Maybe.withDefault "" (List.head range.range)
                                                , Maybe.withDefault "" (List.last range.range)])])]
            , case layoutOptions.yaxis of
                Nothing -> []
                Just range -> [ ("yaxis", E.object [ ("range"
                              , E.list E.float [ Maybe.withDefault 0 (List.head range.range)
                                               , Maybe.withDefault 0 (List.last range.range)])])]
            , case layoutOptions.dragMode of
                Nothing -> []
                Just drag -> [("dragmode", E.string drag )]
            , case layoutOptions.height of
                Nothing -> []
                Just height -> [ ( "height", E.int height ) ]
            ] ))

encodeConfig: ConfiOptions -> E.Value
encodeConfig configOptions =
   (E.object ( List.concat [
                [ ( "displaylogo", E.bool configOptions.displaylogo )
                , ( "displayModeBar", E.bool configOptions.displayModeBar )
                , ( "modeBarButtonsToRemove", E.list E.string configOptions.modeBarButtonsToRemove )
                , ( "responsive", E.bool configOptions.responsive )
                , ( "showlegend", E.bool configOptions.showlegend )]]))


encodeplotargs: String -> List Trace -> LayoutOptions -> ConfiOptions ->E.Value
encodeplotargs div data layoutOptions configOptions =
    E.object
        [ ( "div", E.string div )
        , ( "data", E.list encodetrace data )
        , ( "layout", encodeLayout layoutOptions)
        , ("config", encodeConfig configOptions)
        ]


serializedPlotArgs: String -> List Trace -> LayoutOptions -> ConfiOptions -> String
serializedPlotArgs div data layoutOptions configOptions =
    encodeplotargs div data layoutOptions configOptions |> E.encode 0


-- getdata : PlotQuery query -> Cmd msg
getdata query =
    let
        stringToMaybe : String -> String -> Maybe UB.QueryParameter
        stringToMaybe name value =
            if value == "" then Nothing else Just (UB.string name value)
        fullquery : List UB.QueryParameter
        fullquery = Maybe.values <|
            [ stringToMaybe "name" query.name
            , Maybe.andThen (stringToMaybe "insertion_date") query.idate
            , Just <| UB.int "nocache" query.nocache
            , stringToMaybe "_keep_nans" (if query.keepnans then "true" else "false")
            , stringToMaybe "inferred_freq" (if query.inferredFreq then "true" else "false")
            , stringToMaybe "tzone" query.tzone
            ]
            ++ Maybe.unwrap
            [ stringToMaybe "from_value_date" query.fromdate
            , stringToMaybe "to_value_date" query.todate
            ]
            (\horizonstr -> [ stringToMaybe "horizon" (String.trim horizonstr) ])
            query.horizon
    in
    Http.get
        { url = UB.crossOrigin query.baseurl [ "api", "series", query.apipoint ] fullquery
        , expect = Http.expectString query.callback
        }

-- groups

type alias Group =
    Dict String (Dict String (Maybe Float))


groupdecoder =
    D.dict (D.dict (D.maybe D.float))


getgroupplotdata baseurl name idate callback fromdate todate =
    let
        query = [ UB.string "name" name ]
        fvd = [ UB.string "from_value_date" fromdate ]
        tvd = [ UB.string "to_value_date" todate ]
        fullquery date =
            case date of
                Nothing -> query
                Just d -> List.append query
                          [ UB.string "insertion_date" d ]
    in
    Http.get
        { url = UB.crossOrigin baseurl
              ["api", "group", "state"]
              <| (fullquery idate)
              ++ (if fromdate /= "" then fvd else [])
              ++ (if todate /= "" then tvd else [])
        , expect = Http.expectString callback
        }
