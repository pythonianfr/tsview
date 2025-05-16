module FoldersUtil exposing (..)

import Json.Decode as JD

decodeTree: JD.Decoder ( List String )
decodeTree =
    JD.list JD.string
