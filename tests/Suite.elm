module Suite exposing (testCountKeywords)

import Expect
import KeywordSelector exposing (countKeywords)
import Test exposing (..)


serieNames : List String
serieNames =
    [ "gas.<unknown>.exports.lng.ktd.obs"
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
                        [ ( 0, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( 0, "gas.argentina.exports.lng.ktd.obs" )
                        , ( 0, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( 0, "gas.australia.exports.lng.ktd.obs" )
                        , ( 0, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( 0, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        , test "fcs" <|
            \_ ->
                runCount "fcs"
                    |> Expect.equal
                        [ ( 0, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( 0, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( 0, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        , ( 0, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( 0, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( 0, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( 0, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        ]
        , test "prod fcs" <|
            \_ ->
                runCount "prod fcst"
                    |> Expect.equal
                        [ ( -1, "gas.uk.prod.bacton_seal.mcmd.fcst" )
                        , ( -1, "gas.uk.prod.bacton_shell.mcmd.fcst" )
                        , ( -1, "gas.uk.prod.st_fergus_mobil.mcmd.fcst" )
                        , ( -1, "gas.uk.prod.st_fergus_shell.mcmd.fcst" )
                        , ( 0, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( 0, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( 0, "gas.nl.imports.lng.sendouts.mcmd.monthly.fcst" )
                        ]
        , test "obs ktd arg" <|
            \_ ->
                runCount "obs ktd arg"
                    |> Expect.equal
                        [ ( -2, "gas.argentina.exports.lng.ktd.obs" )
                        , ( -2, "gas.argentina.net_imports.lng.ktd.obs" )
                        , ( -1, "gas.<unknown>.exports.lng.ktd.obs" )
                        , ( -1, "gas.australia.exports.lng.ktd.obs" )
                        , ( -1, "gas.australia.net_imports.lng.ktd.obs" )
                        , ( 0, "gas.namibia.lng.imports.ktd.monthly.fcst" )
                        , ( 0, "gas.nigeria.lng.outages.ktd.monthly.fcst" )
                        , ( 0, "gas.nl.demand.industrial.ktd.full" )
                        , ( 0, "gas.united_arab_emirates.net_exports.lng.das_island.mcmd.obs" )
                        ]
        ]
