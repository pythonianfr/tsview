module Info exposing
    ( getwriteperms
    , idatesdecoder
    )

import Http
import Json.Decode as D
import Url.Builder as UB


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] [ ]
        }


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)
