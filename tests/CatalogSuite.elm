module CatalogSuite exposing
    ( testCatalogParser
    , testNewCatalogParser
    )


import Catalog as C
import Dict exposing ( Dict )
import Json.Decode as JD
import Expect
import Metadata as M
import Series as S
import Set exposing ( Set )
import Test exposing ( Test, test )


cat = """
{"refinery": [
 [
  "meteo.area.at.10u.m.s-1.era5.obs.h",
  "primary"
 ],
 [
  "meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h",
  "primary"
 ]]
}"""


testCatalogParser: Test
testCatalogParser =
    let
        parsed =
            case JD.decodeString C.catalogdecoder cat of
                Err _ -> Dict.empty
                Ok val ->  val

        debugparsed =
            case JD.decodeString C.catalogdecoder cat of
                Err e -> Debug.toString e
                Ok val -> Debug.toString val

        series =
            C.buildseries parsed

        kinds =
            C.buildkinds parsed

        sources =
            C.buildsources parsed
   in
   Test.concat
       [ test "decodecat" <| \_ ->
             Expect.equal debugparsed "Dict.fromList [(\"refinery\",[(\"meteo.area.at.10u.m.s-1.era5.obs.h\",\"primary\"),(\"meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h\",\"primary\")])]"
       , test "buildseries" <| \_ ->
             Expect.equal series [ "meteo.area.at.10u.m.s-1.era5.obs.h"
                                 , "meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h"
                                 ]
       , test "buildkinds" <| \_ ->
             Expect.equal kinds <|
                 Dict.fromList [
                      ( "primary",
                            Set.fromList [ "meteo.area.at.10u.m.s-1.era5.obs.h"
                                         , "meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h"
                                         ]
                      )
                     ]
       , test "buildsources" <| \_ ->
             Expect.equal kinds <|
                 Dict.fromList [
                      ( "primary"
                      , Set.fromList [ "meteo.area.at.10u.m.s-1.era5.obs.h"
                                     , "meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h"
                                     ]
                      )
                     ]
       ]



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

testNewCatalogParser: Test
testNewCatalogParser =
    let
        parsed =
            case JD.decodeString C.rawtsdescdecoder found of
                Err _ -> []
                Ok val -> val

        series =
            C.buildseriesfromdesc parsed

        sources =
            C.buildsourcesfromdesc parsed

    in Test.concat
        [ test "decodetsdesc" <| \_ ->
              Expect.equal parsed
              [ { imeta = Just
                      (Dict.fromList
                           [ ( "contenthash"
                             , M.MString "44a1ea19315ae760e667bb94623c93ff2697e49a"
                             )
                           , ( "formula"
                             , M.MString "(integration 'meteo.start.september' 'meteo.area.de_lu.hdd.deg.era5.obs.d' #:fill #t)" )
                           , ( "index_dtype"
                             , M.MString "<M8[ns]"
                             )
                           , ("index_type"
                             , M.MString "datetime64[ns]"
                             )
                           , ( "tzaware"
                             , M.MBool False
                             )
                           , ( "value_dtype"
                             , M.MString "<f8"
                             )
                           , ( "value_type"
                             , M.MString "float64"
                             )
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
                           , ( "index_dtype"
                             , M.MString "<M8[ns]"
                             )
                           , ( "index_type"
                             , M.MString "datetime64[ns]"
                             )
                           , ( "tzaware"
                             , M.MBool False
                             )
                           , ( "value_dtype"
                             , M.MString "<f8"
                             )
                           , ( "value_type"
                             , M.MString "float64"
                             )
                           ]
                      )
                , kind = "formula"
                , meta = Just (Dict.fromList [])
                , name = "meteo.area.fr.hdd-cumsum.deg.era5.obs.d"
                , source = "local"
                }
              ]
        , test "buildseriesfromdesc" <| \_ ->
            Expect.equal series [ "meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d"
                                , "meteo.area.fr.hdd-cumsum.deg.era5.obs.d"
                                ]
        , test "buildsourcesfromdesc" <| \_ ->
            Expect.equal sources <|
                Dict.fromList
                    [
                     ( "local"
                     , Set.fromList
                         [ "meteo.area.de_lu.hdd-cumsum.deg.era5.obs.d"
                         , "meteo.area.fr.hdd-cumsum.deg.era5.obs.d"
                         ]
                     )
                    ]
        ]
