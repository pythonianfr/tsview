module Info exposing
    ( getwriteperms
    , idatesdecoder
    , metatype
    )

import Http
import Json.Decode as D
import Metadata as M
import Url.Builder as UB


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] [ ]
        }


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)


metatype val =
    case val of
        Nothing -> "virt"
        Just x ->
            case x of
                M.MString _ -> "str"
                M.MInt _ -> "int"
                M.MFloat _ -> "float"
                M.MBool _ -> "bool"
                M.MList _ -> "list"
