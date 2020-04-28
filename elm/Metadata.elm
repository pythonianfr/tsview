module Metadata exposing (MetaVal(..)
                         , decodemeta
                         , decodemetaval
                         , getmetadata
                         , metanames
                         , metavaltostring
                         , UserMetadata
                         , StdMetadata)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Url.Builder as UB


metanames =
    [ "tzaware"
    , "index_type"
    , "index_dtype"
    , "value_type"
    , "value_dtype"
    , "supervision_status" -- hijacked to detect formula-ness
    , "index_names" -- deprecated but might still be there
    ]


type MetaVal
    = MString String
    | MInt Int
    | MFloat Float
    | MBool Bool
    | MList (List MetaVal)


showbool b =
    if b then "true" else "false"


metavaltostring mv =
    case mv of
        MString s -> s
        MInt i -> String.fromInt i
        MFloat f -> String.fromFloat f
        MBool b -> showbool b
        MList l -> String.join ", " <| List.map metavaltostring l


type alias StdMetadata =
    Dict String MetaVal


type alias UserMetadata =
    Dict String MetaVal


getmetadata urlprefix name callback =
    Http.get
        { expect =
              Http.expectString callback
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "metadata" ]
                [ UB.string "name" name
                , UB.int "all" 1 ]
        }

decodemetaval : D.Decoder MetaVal
decodemetaval =
    D.oneOf
        [ D.map MString D.string
        , D.map MInt D.int
        , D.map MFloat D.float
        , D.map MBool D.bool
        -- terminal unprocessed node (only for the deprecated index_names entry)
        , D.map MList (D.list (D.lazy (\_  -> decodemetaval)))
        ]


decodemeta : D.Decoder UserMetadata
decodemeta =
    D.dict decodemetaval
