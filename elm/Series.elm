module Series exposing
    ( nullseries
    , Series
    , seriesdecoder
    , serieslistdecoder
    )


import Json.Decode as JD
import Metadata as M


type alias Series =
    { name : String
    , source : String
    , kind : String
    , imeta : Maybe M.StdMetadata
    , meta : Maybe M.StdMetadata
    }


nullseries =
    { name = "<unknown>"
    , source = "<unknown>"
    , kind = "<unknown>"
    , imeta = Nothing
    , meta = Nothing
    }


seriesdecoder =
    JD.map5 Series
        ( JD.field "name" JD.string )
        ( JD.field "source" JD.string )
        ( JD.field "kind" JD.string )
        ( JD.field "imeta" (JD.nullable M.decodemeta) )
        ( JD.field "meta" (JD.nullable M.decodemeta) )


serieslistdecoder =
    JD.list seriesdecoder
