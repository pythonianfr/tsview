module Plotter exposing
    ( defaultoptions
    , defaultLayoutOptions
    , getdata
    , getgroupplotdata
    , Group
    , groupdecoder
    , scatterplot
    , plotargs
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

-- series

type alias LayoutOptions =
    { title: String
    , dragMode: String
    , xaxis: Maybe { range: List String }
    }


defaultLayoutOptions =
    { title = ""
    , dragMode = "zoom"
    , xaxis = Nothing
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


encodeplotargs: String -> List Trace -> LayoutOptions -> E.Value
encodeplotargs div data layoutOptions =
    E.object
        [ ( "div", E.string div )
        , ( "data", E.list encodetrace data )
        , ( "layout", (E.object ( [ ("title", E.string layoutOptions.title)
                                , ("dragmode", E.string layoutOptions.dragMode)
                                ] ++
            case layoutOptions.xaxis of
                Nothing -> []
                Just range -> [ ("xaxis", E.object [ ("range"
                              , E.list E.string [ Maybe.withDefault "" (List.head range.range)
                                                , Maybe.withDefault "" (List.last range.range)])])]
        )))
        ]


plotargs: String -> List Trace -> LayoutOptions -> String
plotargs div data layoutOptions =
    encodeplotargs div data layoutOptions |> E.encode 0


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
