module MetadataSuite exposing
    ( testMetadataParser )


import Expect
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
                Err e -> Debug.toString e
                Ok val -> Debug.toString val
   in
     test "decodemeta" <| \_ ->
         Expect.equal parsed "Dict.fromList [(\"contenthash\",MString \"44a1ea19315ae760e667bb94623c93ff2697e49a\"),(\"formula\",MString \"(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)\"),(\"index_dtype\",MString \"<M8[ns]\"),(\"index_type\",MString \"datetime64[ns]\"),(\"tzaware\",MBool False),(\"value_dtype\",MString \"<f8\"),(\"value_type\",MString \"float64\")]"
