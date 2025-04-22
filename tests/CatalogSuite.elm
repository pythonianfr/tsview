module CatalogSuite exposing
    ( testCatalogParser )


import Catalog as C
import Json.Decode as JD
import Expect
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
                Err e -> Debug.toString e
                Ok val -> Debug.toString val
   in
     test "decodecat" <| \_ ->
         Expect.equal parsed "Dict.fromList [(\"refinery\",[(\"meteo.area.at.10u.m.s-1.era5.obs.h\",\"primary\"),(\"meteo.area.at.10u.m.s-1.hres.ecmwf.fcst.3-6h\",\"primary\")])]"
