module EdiTableSuite exposing
    ( testPatchCurrent
    , testMergeData
    , testCartesianData
    , testParsePasted
    )

import Dict exposing (Dict)
import EdiTable exposing (..)
import Expect
import Test exposing ( Test, test )


-- Test data setup
baseEntry : Entry
baseEntry =
    { raw = Just "10.5"
    , value = MFloat (Just 10.5)
    , edition = NoEdition
    , editable = True
    , override = False
    , indexRow = ""
    , indexCol = "test"
    , fromBatch = False
    }

createBaseData : Dict String Payload
createBaseData =
    Dict.fromList
        [ ("2024-01-01T00:00:00"
          , Complex { baseEntry | indexRow = "2024-01-01T00:00:00", raw = Just "100.0", value = MFloat (Just 100.0)})
        , ("2024-01-02T00:00:00"
          , Complex { baseEntry | indexRow = "2024-01-02T00:00:00", raw = Just "200.0", value = MFloat (Just 200.0)})
        , ("2024-01-03T00:00:00"
          , Complex { baseEntry | indexRow = "2024-01-03T00:00:00", raw = Just "300.0", value = MFloat (Just 300.0)})
        , ("2024-01-05T00:00:00"
          , Complex { baseEntry | indexRow = "2024-01-05T00:00:00", raw = Just "500.0", value = MFloat (Just 500.0)})
        ]

createPatchData : Dict String Payload
createPatchData =
    Dict.fromList
        [ ("2024-01-02T00:00:00", Scalar (MFloat (Just 250.0)))  -- overlaps with base, should update
        , ("2024-01-03T00:00:00", Scalar (MFloat Nothing))       -- overlaps with base, Nothing should delete
        , ("2024-01-04T00:00:00", Scalar (MFloat (Just 400.0)))  -- new key, should add
        , ("2024-01-06T00:00:00", Scalar (MFloat Nothing))       -- new key with Nothing, should add with deletion marker
        ]

testPatchCurrent : Test
testPatchCurrent =
    test "patchCurrent merges base and patch data correctly" <|
        \_ ->
            let
                base = createBaseData
                patch = createPatchData
                seriesName = "test-series"
                result = patchCurrent base patch seriesName
                -- Check that result contains expected keys
                expectedKeys = [ "2024-01-01T00:00:00"
                               , "2024-01-02T00:00:00"
                               , "2024-01-03T00:00:00"
                               , "2024-01-04T00:00:00"
                               , "2024-01-05T00:00:00"
                               , "2024-01-06T00:00:00"
                               ]
                actualKeys = Dict.keys result |> List.sort
                -- Check specific entries
                jan01Entry = Dict.get "2024-01-01T00:00:00" result
                jan02Entry = Dict.get "2024-01-02T00:00:00" result
                jan03Entry = Dict.get "2024-01-03T00:00:00" result
                jan04Entry = Dict.get "2024-01-04T00:00:00" result
                jan05Entry = Dict.get "2024-01-05T00:00:00" result
                jan06Entry = Dict.get "2024-01-06T00:00:00" result
            in
            Expect.all
                [ \_ -> Expect.equal (List.sort expectedKeys) actualKeys

                -- Jan 01: should remain unchanged (base only)
                , \_ ->
                    case jan01Entry of
                        Just (Complex entry) ->
                            Expect.all
                                [ \_ -> Expect.equal "100.0" (Maybe.withDefault "" entry.raw)
                                , \_ ->Expect.equal (MFloat (Just 100.0)) entry.value
                                ] ()
                        _ -> Expect.fail "Jan 01 should be Complex entry with original value"

                -- Jan 02: should be updated with patch value
                , \_ ->
                    case jan02Entry of
                        Just (Complex entry) ->
                            Expect.all
                                [ \_ -> Expect.equal "250" (Maybe.withDefault "" entry.raw)
                                , \_ ->Expect.equal (Edition (MFloat 250.0)) entry.edition
                                ] ()
                        _ -> Expect.fail "Jan 02 should be Complex entry with updated value"

                -- Jan 03: should be marked for deletion (patch had Nothing)
                , \_ ->
                    case jan03Entry of
                        Just (Complex entry) ->
                            Expect.equal Deletion entry.edition
                        _ -> Expect.fail "Jan 03 should remain as Complex entry"

                -- Jan 04: should be new entry from patch
                , \_ ->
                    case jan04Entry of
                        Just (Complex entry) ->
                            Expect.all
                                [ \_ -> Expect.equal "400" (Maybe.withDefault "" entry.raw)
                                , \_ ->Expect.equal (Edition (MFloat 400.0)) entry.edition
                                , \_ ->Expect.equal True entry.fromBatch
                                ] ()
                        _ -> Expect.fail "Jan 04 should be new Complex entry"

                -- Jan 05: should remain unchanged (base only)
                , \_ ->
                    case jan05Entry of
                        Just (Complex entry) ->
                            Expect.equal "500.0" (Maybe.withDefault "" entry.raw)
                        _ -> Expect.fail "Jan 05 should remain unchanged"

                -- Jan 06: should be new entry with deletion marker (patch had Nothing)
                , \_ ->
                    case jan06Entry of
                        Just (Complex entry) ->
                            Expect.all
                                [ \_ -> Expect.equal Deletion entry.edition
                                , \_ ->Expect.equal True entry.fromBatch
                                ] ()
                        _ -> Expect.fail "Jan 06 should be new Complex entry with deletion marker"
                ]
                ()


-- Test data for mergeData
createTestComponents : List Component
createTestComponents =
    let
        -- Component 1: Primary type with float values
        comp1Data = Dict.fromList
            [ ("2024-01-01T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-01T00:00:00", indexCol = "temperature", raw = Just "20.5", value = MFloat (Just 20.5) })
            , ("2024-01-02T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-02T00:00:00", indexCol = "temperature", raw = Just "22.1", value = MFloat (Just 22.1) })
            , ("2024-01-03T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-03T00:00:00", indexCol = "temperature", raw = Just "19.8", value = MFloat (Just 19.8) })
            ]
        -- Component 2: Formula type with some overlapping dates
        comp2Data = Dict.fromList
            [ ("2024-01-02T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-02T00:00:00", indexCol = "humidity", raw = Just "65.0", value = MFloat (Just 65.0) })
            , ("2024-01-03T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-03T00:00:00", indexCol = "humidity", raw = Just "70.2", value = MFloat (Just 70.2) })
            , ("2024-01-04T00:00:00"
               , Complex { baseEntry | indexRow = "2024-01-04T00:00:00", indexCol = "humidity", raw = Just "68.5", value = MFloat (Just 68.5) })
            ]
        -- Component 3: Auto type with string values and different dates
        comp3Data = Dict.fromList
            [ ("2024-01-01T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-01T00:00:00", indexCol = "weather", raw = Just "sunny", value = MString (Just "sunny") })
            , ("2024-01-04T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-04T00:00:00", indexCol = "weather", raw = Just "cloudy", value = MString (Just "cloudy") })
            , ("2024-01-05T00:00:00"
              , Complex { baseEntry | indexRow = "2024-01-05T00:00:00", indexCol = "weather", raw = Just "rainy", value = MString (Just "rainy") })
            ]
    in
    [ { name = "temperature"
      , cType = Primary
      , data = comp1Data
      , tzaware = False
      , status = CompLoaded
      }
    , { name = "humidity"
      , cType = Formula
      , data = comp2Data
      , tzaware = False
      , status = CompLoaded
      }
    , { name = "weather"
      , cType = Auto
      , data = comp3Data
      , tzaware = True
      , status = CompLoaded
      }
    ]


-- Helper to convert Stuff to String representation
stuffToString : Stuff -> String
stuffToString stuff =
    case stuff of
        DateRow date -> date
        Header (name, cType) ->
            let typeStr = case cType of
                    Primary -> "Primary"
                    Formula -> "Formula"
                    Auto -> "Auto"
            in name ++ " (" ++ typeStr ++ ")"
        Cell entry -> Maybe.withDefault "" entry.raw


testMergeData : Test
testMergeData =
    test "mergeData creates correct grid structure as list of list of strings" <|
        \_ ->
            let
                components = createTestComponents
                result = mergeData components
                -- Convert result to list of list of strings
                stringResult = List.map (List.map stuffToString) result
                -- Expected result as list of list of strings
                expectedResult =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Auto)" ]  -- Header row
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny" ]        -- Row 1: has temp and weather
                    , [ "2024-01-02T00:00:00", "22.1", "65", "" ]           -- Row 2: has temp and humidity
                    , [ "2024-01-03T00:00:00", "19.8", "70.2", "" ]         -- Row 3: has temp and humidity
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy" ]       -- Row 4: has humidity and weather
                    , [ "2024-01-05T00:00:00", "", "", "rainy" ]            -- Row 5: has only weather
                    ]
            in
            Expect.equal expectedResult stringResult


testCartesianData : Test
testCartesianData =
    test "cartesianData converts mergeData output to coordinate-based dictionary" <|
        \_ ->
            let
                components = createTestComponents
                mergeResult = mergeData components
                cartesianResult = cartesianData mergeResult

                -- Convert the cartesian result to a list of (coordinate, string_value) pairs for testing
                coordinateValuePairs =
                    cartesianResult
                    |> Dict.toList
                    |> List.map (\((row, col), stuff) -> ((row, col), stuffToString stuff))
                    |> List.sortBy (\((r, c), _) -> (r, c))  -- Sort by coordinates for predictable testing

                -- Expected coordinates and their string values (cartesianData starts at (-1, -1))
                expectedCoordinates =
                    [ ((-1, -1), "Dates (Primary)")           -- Header row
                    , ((-1, 0), "temperature (Primary)")
                    , ((-1, 1), "humidity (Formula)")
                    , ((-1, 2), "weather (Auto)")

                    , ((0, -1), "2024-01-01T00:00:00")       -- Row 1: 2024-01-01
                    , ((0, 0), "20.5")
                    , ((0, 1), "")
                    , ((0, 2), "sunny")

                    , ((1, -1), "2024-01-02T00:00:00")       -- Row 2: 2024-01-02
                    , ((1, 0), "22.1")
                    , ((1, 1), "65")
                    , ((1, 2), "")

                    , ((2, -1), "2024-01-03T00:00:00")       -- Row 3: 2024-01-03
                    , ((2, 0), "19.8")
                    , ((2, 1), "70.2")
                    , ((2, 2), "")

                    , ((3, -1), "2024-01-04T00:00:00")       -- Row 4: 2024-01-04
                    , ((3, 0), "")
                    , ((3, 1), "68.5")
                    , ((3, 2), "cloudy")

                    , ((4, -1), "2024-01-05T00:00:00")       -- Row 5: 2024-01-05
                    , ((4, 0), "")
                    , ((4, 1), "")
                    , ((4, 2), "rainy")
                    ]

                allKeys = Dict.keys cartesianResult |> List.sort
            in
            Expect.all
                [ \_ -> Expect.equal 24 (Dict.size cartesianResult)
                , \_ -> Expect.equal (List.map Tuple.first expectedCoordinates) allKeys
                , \_ -> Expect.equal expectedCoordinates coordinateValuePairs
                ]
                ()


testParsePasted : Test
testParsePasted =
    test "parsePasted converts spreadsheet paste data to list of list of strings" <|
        \_ ->
            let
                pastedData = "12.5\t34.7\n89.1\t56.2"
                -- Test with isString = False (numerical data, spaces should be removed)
                result = parsePasted pastedData False
                expectedResult =
                    [ [ "12.5", "34.7" ]    -- First row
                    , [ "89.1", "56.2" ]    -- Second row
                    ]

                -- Test with different line endings (Windows format)
                windowsData = "12.5\t34.7\r\n89.1\t56.2"
                windowsResult = parsePasted windowsData False

                -- Test with isString = True (string data, spaces should be preserved)
                stringData = "Hello World\tGood Morning\nTest Data\tMore Text"
                stringResult = parsePasted stringData True
                expectedStringResult =
                    [ [ "Hello World", "Good Morning" ]
                    , [ "Test Data", "More Text" ]
                    ]

                -- Test single row (no line breaks)
                singleRowData = "45.8\t67.3"
                singleRowResult = parsePasted singleRowData False
                expectedSingleRow = [ [ "45.8", "67.3" ] ]

                -- Test with spaces in numerical data (should be removed)
                spacedData = "1 2.5\t3 4.7\n8 9.1\t5 6.2"
                spacedResult = parsePasted spacedData False
                expectedSpacedResult =
                    [ [ "12.5", "34.7" ]
                    , [ "89.1", "56.2" ]
                    ]

                -- Setting starting corner at (0, 0) instead of (-1, -1)
                cartesianResult = cartesianDataRec result [] 0 0 0 Dict.empty

                expectedCartesian =
                    [ ((0, 0), "12.5")
                    , ((0, 1), "34.7")
                    , ((1, 0), "89.1")
                    , ((1, 1), "56.2")
                    ]

                -- Convert cartesian result to sorted list for comparison
                cartesianList =
                    cartesianResult
                    |> Dict.toList
                    |> List.sortBy (\((r, c), _) -> (r, c))

            in
            Expect.all
                [ \_ -> Expect.equal expectedResult result

                , -- Test Windows line endings
                  \_ -> Expect.equal expectedResult windowsResult

                , -- Test string data (spaces preserved)
                  \_ -> Expect.equal expectedStringResult stringResult

                , -- Test single row
                  \_ -> Expect.equal expectedSingleRow singleRowResult

                , -- Test space removal for numerical data
                  \_ -> Expect.equal expectedSpacedResult spacedResult

                , -- Test complete cartesian mapping
                  \_ -> Expect.equal expectedCartesian cartesianList
                ]
                ()
