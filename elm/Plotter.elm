module Plotter exposing
    ( getplotdata
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
import Maybe.Extra as Maybe
import Url.Builder as UB

-- series

type alias Trace =
    { type_ : String
    , name : String
    , x : List String
    , y : List Float
    , mode : String
    }


type alias CallBack msg = (Result Http.Error String -> msg)


type alias PlotData msg =
    { baseurl : String
    , name : String
    , idate : Maybe String
    , callback : CallBack msg
    , nocache : Int
    , fromdate : String
    , todate : String
    , horizon : Maybe String
    }


encodetrace : Trace -> E.Value
encodetrace t =
    E.object
        [ ( "type", E.string t.type_ )
        , ( "name", E.string t.name )
        , ( "x", E.list E.string t.x )
        , ( "y", E.list E.float t.y )
        , ( "mode", E.string t.mode )
        ]


type alias Series =
    Dict String Float


seriesdecoder =
    D.dict D.float


type alias TraceArgs =
    String -> List String -> List Float -> String -> Trace


scatterplot : TraceArgs
scatterplot =
    Trace "scatter"


encodeplotargs div data =
    E.object
        [ ( "div", E.string div )
        , ( "data", E.list encodetrace data )
        ]


plotargs div data =
    encodeplotargs div data |> E.encode 0


getplotdata : PlotData msg -> Cmd msg
getplotdata data =
    let
        stringToMaybe : String -> String -> Maybe UB.QueryParameter
        stringToMaybe name value =
            if value == "" then Nothing else Just (UB.string name value)
        fullquery : List UB.QueryParameter
        fullquery = Maybe.values <|
            [ stringToMaybe "name" data.name
            , Maybe.andThen (stringToMaybe "insertion_date") data.idate
            , Just <| UB.int "nocache" data.nocache
            ]
            ++ Maybe.unwrap
            [ stringToMaybe "from_value_date" data.fromdate
            , stringToMaybe "to_value_date" data.todate
            ]
            (\horizonstr -> [stringToMaybe "horizon" (String.trim horizonstr)])
            data.horizon
    in Http.get
    { url = UB.crossOrigin data.baseurl ["api", "series", "state"] fullquery
    , expect = Http.expectString data.callback
    }

-- groups

type alias Group =
    Dict String (Dict String Float)


groupdecoder =
    D.dict (D.dict D.float)


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
