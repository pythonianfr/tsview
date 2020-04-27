module Plotter exposing (getplotdata, scatterplot, plotargs, seriesdecoder, Series)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder as UB


type alias Trace =
    { type_ : String
    , name : String
    , x : List String
    , y : List Float
    , mode : String
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
    Dict.Dict String Float


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


getplotdata baseurl name idate callback =
    let
        query = [ UB.string "name" name ]
        fullquery date =
            case date of
                Nothing -> query
                Just d -> List.append query
                          [ UB.string "insertion_date" d ]
    in
    Http.get
        { url = UB.crossOrigin baseurl
              ["api", "series", "state"]
              (fullquery idate)
        , expect = Http.expectString callback
        }
