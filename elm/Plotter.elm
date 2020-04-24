module Plotter exposing (scatterplot, plotargs)

import Http
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


getdata model name callback =
    Http.get
        { url = UB.crossOrigin model.baseurl
              ["api", "series", "state"]
              [ UB.string "name" name ]
        , expect = callback
        }
