module CatalogSuite exposing
    ( testCatalogParser )


import Catalog as C
import Dict exposing ( Dict )
import Json.Decode as JD
import Expect
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
       ]
