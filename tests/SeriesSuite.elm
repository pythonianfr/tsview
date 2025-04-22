module SeriesSuite exposing
    ( testSeriesListParser
    , testSeriesParser
    )

import Json.Decode as JD
import Expect
import Series as S
import Test exposing ( Test, test )


input1series = """
{
        "name": "meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d",
        "imeta": {
            "formula": "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)",
            "tzaware": false,
            "index_type": "datetime64[ns]",
            "value_type": "float64",
            "contenthash": "44a1ea19315ae760e667bb94623c93ff2697e49a",
            "index_dtype": "<M8[ns]",
            "value_dtype": "<f8"
        },
        "meta": {},
        "source": "local",
        "kind": "formula"
    }
"""

input2series = """
[
    {
        "name": "meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d",
        "imeta": {
            "formula": "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)",
            "tzaware": false,
            "index_type": "datetime64[ns]",
            "value_type": "float64",
            "contenthash": "44a1ea19315ae760e667bb94623c93ff2697e49a",
            "index_dtype": "<M8[ns]",
            "value_dtype": "<f8"
        },
        "meta": {},
        "source": "local",
        "kind": "formula"
    },
    {
        "name": "meteo.area.fr.hdd-cumsum.deg.era5.obs.d",
        "imeta": {
            "formula": "(integration 'meteo.start.september' 'meteo.area.fr.hdd.deg.era5.obs.d' #:fill #t)",
            "tzaware": false,
            "index_type": "datetime64[ns]",
            "value_type": "float64",
            "contenthash": "5f5abca5dd313c3b4181d0a422b6ceb448249fd1",
            "index_dtype": "<M8[ns]",
            "value_dtype": "<f8"
        },
        "meta": {},
        "source": "local",
        "kind": "formula"
    }
] """


testSeriesParser: Test
testSeriesParser =
    let
        parsed = case JD.decodeString S.seriesdecoder input1series of
                Err e -> Debug.toString e
                Ok val -> Debug.toString val
   in
     test "decodeseries" <| \_ ->
         Expect.equal parsed "{ imeta = Just (Dict.fromList [(\"contenthash\",MString \"44a1ea19315ae760e667bb94623c93ff2697e49a\"),(\"formula\",MString \"(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)\"),(\"index_dtype\",MString \"<M8[ns]\"),(\"index_type\",MString \"datetime64[ns]\"),(\"tzaware\",MBool False),(\"value_dtype\",MString \"<f8\"),(\"value_type\",MString \"float64\")]), kind = \"formula\", meta = Just (Dict.fromList []), name = \"meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d\", source = \"local\" }"


testSeriesListParser: Test
testSeriesListParser =
    let
        parsed =
            case JD.decodeString S.serieslistdecoder input2series of
                Err e -> Debug.toString e
                Ok val -> Debug.toString val
   in
   test "decodeserieslist" <| \_ ->
       Expect.equal parsed "[{ imeta = Just (Dict.fromList [(\"contenthash\",MString \"44a1ea19315ae760e667bb94623c93ff2697e49a\"),(\"formula\",MString \"(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)\"),(\"index_dtype\",MString \"<M8[ns]\"),(\"index_type\",MString \"datetime64[ns]\"),(\"tzaware\",MBool False),(\"value_dtype\",MString \"<f8\"),(\"value_type\",MString \"float64\")]), kind = \"formula\", meta = Just (Dict.fromList []), name = \"meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d\", source = \"local\" },{ imeta = Just (Dict.fromList [(\"contenthash\",MString \"5f5abca5dd313c3b4181d0a422b6ceb448249fd1\"),(\"formula\",MString \"(integration 'meteo.start.september' 'meteo.area.fr.hdd.deg.era5.obs.d' #:fill #t)\"),(\"index_dtype\",MString \"<M8[ns]\"),(\"index_type\",MString \"datetime64[ns]\"),(\"tzaware\",MBool False),(\"value_dtype\",MString \"<f8\"),(\"value_type\",MString \"float64\")]), kind = \"formula\", meta = Just (Dict.fromList []), name = \"meteo.area.fr.hdd-cumsum.deg.era5.obs.d\", source = \"local\" }]"
