module MetadataSuite exposing
    ( testMetadataParser
    , testOldmetaParser
    )

import Dict exposing (Dict)
import Expect
import Info as I
import Json.Decode as JD
import Metadata as M
import Test exposing ( Test, test )


metadata = """
{"formula": "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)",
 "tzaware": false,
 "index_type": "datetime64[ns]",
 "value_type": "float64",
 "contenthash": "44a1ea19315ae760e667bb94623c93ff2697e49a",
 "index_dtype": "<M8[ns]",
 "value_dtype": "<f8"
}"""


testMetadataParser: Test
testMetadataParser =
    let
        parsed = case JD.decodeString M.decodemeta metadata of
                Err e -> Dict.empty
                Ok val -> val
   in
     test "decodemeta" <| \_ ->
         Expect.equal parsed <|
             Dict.fromList
                 [ ( "contenthash", M.MString "44a1ea19315ae760e667bb94623c93ff2697e49a" )
                 , ( "formula", M.MString "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)")
                 , ( "index_dtype", M.MString "<M8[ns]" )
                 , ( "index_type", M.MString "datetime64[ns]")
                 , ("tzaware", M.MBool False)
                 , ( "value_dtype", M.MString "<f8")
                 , ( "value_type", M.MString "float64" )
                 ]


oldmeta = """
[["2025-05-15T15:06:57.292528+00:00", {}, "no-user"], ["2025-05-15T15:06:57.297810+00:00", {"freq": "d", "description": "banana spot price"}, "no-user"]]
"""

testOldmetaParser: Test
testOldmetaParser =
    let
        parsed =
            case JD.decodeString I.oldmetasdecoder oldmeta of
                Err e -> []
                Ok val -> val
    in
    test "decodeoldmetas" <| \_ ->
        Expect.equal parsed
            [ { metadata = Dict.fromList []
              , stamp = "2025-05-15T15:06:57.292528+00:00"
              , user = "no-user"
              }
            , { metadata = Dict.fromList
                    [ ( "description", M.MString "banana spot price" )
                    , ( "freq", M.MString "d" )
                    ]
              , stamp = "2025-05-15T15:06:57.297810+00:00"
              , user = "no-user"
              }
            ]
