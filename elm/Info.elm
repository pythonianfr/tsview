module Info exposing
    (idatesdecoder)

import Json.Decode as D


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)
