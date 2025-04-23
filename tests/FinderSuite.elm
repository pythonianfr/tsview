module FinderSuite exposing
    ( testParser )


import Json.Decode as JD
import Dict exposing ( Dict )
import Expect
import Finder as F
import Metadata as M
import Series as S
import Set exposing ( Set )
import Test exposing ( Test, test )


found = """
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
]
"""


testParser: Test
testParser =
    let
        parsed =
            case JD.decodeString F.rawtsdescdecoder found of
                Err _ -> []
                Ok val -> val

    in
    test "decodetsdesc" <| \_ ->
        Expect.equal parsed
            [ { imeta = Just
                    (Dict.fromList
                         [ ( "contenthash"
                           , M.MString "44a1ea19315ae760e667bb94623c93ff2697e49a"
                           )
                         , ( "formula"
                           , M.MString "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)" )
                         , ( "index_dtype", M.MString "<M8[ns]" )
                         , ( "index_type", M.MString "datetime64[ns]" )
                         , ( "tzaware", M.MBool False )
                         , ( "value_dtype", M.MString "<f8" )
                         , ( "value_type", M.MString "float64" )
                         ]
                    )
              , kind = "formula"
              , meta = Just (Dict.fromList [])
              , name = "meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d"
              , source = "local"
              }
            , { imeta = Just
                    (Dict.fromList
                         [ ( "contenthash"
                           , M.MString "5f5abca5dd313c3b4181d0a422b6ceb448249fd1"
                           )
                         , ( "formula"
                           , M.MString "(integration 'meteo.start.september' 'meteo.area.fr.hdd.deg.era5.obs.d' #:fill #t)" )
                         , ( "index_dtype", M.MString "<M8[ns]" )
                         , ( "index_type" , M.MString "datetime64[ns]" )
                         , ( "tzaware", M.MBool False )
                         , ( "value_dtype", M.MString "<f8" )
                         , ( "value_type", M.MString "float64" )
                         ]
                    )
              , kind = "formula"
              , meta = Just (Dict.fromList [])
              , name = "meteo.area.fr.hdd-cumsum.deg.era5.obs.d"
              , source = "local"
              }
            ]
