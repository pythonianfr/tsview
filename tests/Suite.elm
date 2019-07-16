module Suite exposing (testCountKeywords)

import Expect
import KeywordSelector exposing (countKeywords)
import Test exposing (..)


serieNames : List String
serieNames =
    [ "gas.<unknown>.exports.lng.ktd.obs"
    , "gas.argentina.full"
    , "gas.price.everyday.ttf.penceth"
    , "gas.uk.prod.st_fergus_shell.mcmd.fcst"
    , "gas.uk.prod.bacton_shell.mcmd.fcst"
    , "gas.uk.prod.st_fergus_mobil.mcmd.fcst"
    , "gas.uk.prod.bacton_seal.mcmd.fcst"
    , "gas.australia.exports.lng.ktd.obs"
    , "27553_M_10"
    , "27553_M_0"
    , "38755_M_0"
    , "27917_M_31"
    , "27553_M_12"
    , "27454_M_2019-10-01000000"
    , "27553_M_1"
    , "27917_M_33"
    , "38755_D_0"
    , "gas.australia.net_imports.lng.ktd.obs"
    , "gas.nigeria.lng.outages.ktd.monthly.fcst"
    , "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst"
    , "gas.namibia.lng.imports.ktd.monthly.fcst"
    , "gas.argentina.net_imports.lng.ktd"
    , "gas.argentina.exports.lng.ktd"
    , "gas.nl.demand.industrial.ktd.full"
    , "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs"
    , "gas.argentina.exports.lng.ktd.obs"
    , "gas.argentina.net_imports.lng.ktd.obs"
    ]


testCountKeywords : Test
testCountKeywords =
    let
        runCount x =
            countKeywords x (String.split ".") serieNames
    in
    describe "test selector"
        [ test "obs" <|
            \_ ->
                runCount "obs"
                    |> Expect.equal
                        [ ( -166667, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( -166667, "gas.argentina.exports.lng.ktd.obs" )
                        , ( -166667, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( -166667, "gas.australia.exports.lng.ktd.obs" )
                        , ( -166667, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( -142857, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        , test "fcs" <|
            \_ ->
                runCount "fcs"
                    |> Expect.equal
                        [ ( -125000, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( -125000, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( -125000, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( -125000, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        , ( -107143, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -107143, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -93750, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        ]
        , test "prod fcs" <|
            \_ ->
                runCount "prod fcs"
                    |> Expect.equal
                        [ ( -291667, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( -291667, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( -291667, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( -291667, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        , ( -107143, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -107143, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -93750, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        ]
        , test "obs ktd arge" <|
            \_ ->
                runCount "obs ktd arge"
                    |> Expect.equal
                        [ ( -407407, "gas.argentina.exports.lng.ktd.obs" )
                        , ( -407407, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( -333333, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( -333333, "gas.australia.exports.lng.ktd.obs" )
                        , ( -333333, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( -288889, "gas.argentina.exports.lng.ktd" )
                        , ( -288889, "gas.argentina.net_imports.lng.ktd" )
                        , ( -166667, "gas.nl.demand.industrial.ktd.full" )
                        , ( -148148, "gas.argentina.full" )
                        , ( -142857, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -142857, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -142857, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        ]
