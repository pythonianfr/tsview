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
            countKeywords x serieNames
    in
    describe "test selector"
        [ test "obs" <|
            \_ ->
                runCount "obs"
                    |> Expect.equal
                        [ ( -1009091, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( -1009091, "gas.argentina.exports.lng.ktd.obs" )
                        , ( -1009091, "gas.australia.exports.lng.ktd.obs" )
                        , ( -1008108, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( -1008108, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( -1005000, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        , test "fcs" <|
            \_ ->
                runCount "fcs"
                    |> Expect.equal
                        [ ( -1009091, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( -1008824, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( -1008108, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( -1008108, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        , ( -1007500, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -1007500, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -1006667, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        ]
        , test "prod fcs" <|
            \_ ->
                runCount "prod fcs"
                    |> Expect.equal
                        [ ( -2021212, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( -2020589, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( -2018919, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( -2018919, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        , ( -1007500, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -1007500, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -1006667, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        ]
        , test "obs ktd arge" <|
            \_ ->
                runCount "obs ktd arge"
                    |> Expect.equal
                        [ ( -3030303, "gas.argentina.exports.lng.ktd.obs" )
                        , ( -3027027, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( -2024138, "gas.argentina.exports.lng.ktd" )
                        , ( -2021212, "gas.argentina.net_imports.lng.ktd" )
                        , ( -2018182, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( -2018182, "gas.australia.exports.lng.ktd.obs" )
                        , ( -2016216, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( -1022222, "gas.argentina.full" )
                        , ( -1009091, "gas.nl.demand.industrial.ktd.full" )
                        , ( -1007500, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( -1007500, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( -1005000, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        ]
