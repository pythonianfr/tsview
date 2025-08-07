module EdiTableSuite exposing
    ( testPatchCurrent
    , testMergeData
    , testCartesianData
    , testParsePasted
    , testPasteRectangle
    )

import Dict exposing (Dict)
import EdiTable exposing (..)
import Expect
import Test exposing ( Test, test )

-- Primary components must be defined as BaseSupervision/BaseSupervisionString,
-- then transformed to Complex before storage in Component record.
-- Formula and Auto series are stored as Scalar in Component record.


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
    let
        -- Primary component data should use BaseSupervision with override=False, then transform to Complex
        baseSupervisionData = Dict.fromList
            [ ("2024-01-01T00:00:00", { value = Just 100.0, override = False })
            , ("2024-01-02T00:00:00", { value = Just 200.0, override = False })
            , ("2024-01-03T00:00:00", { value = Just 300.0, override = False })
            , ("2024-01-05T00:00:00", { value = Just 500.0, override = False })
            ]
    in
    Dict.map (\_ bS -> baseToEntry bS) baseSupervisionData

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

                -- Extract all result entries as explicit Elm records for visibility
                resultEntries =
                    result
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map (\(key, payload) ->
                        case payload of
                            Complex entry -> (key, entry)
                            Scalar _ -> (key, baseEntry) -- fallback, shouldn't happen
                    )

                -- the inconsistencies in the indexCol and indexRow come from the face
                -- that we don't pass through mergeData which is the place that ensure
                -- the completion of these fields.
                -- in here, only the path "rightOnly" in patchCurrent will be completed
                expectedEntries =
                    [("2024-01-01T00:00:00",{ editable = True, edition = NoEdition, fromBatch = False, indexCol = "", indexRow = "", override = False, raw = Just "100", value = MFloat (Just 100) })
                    ,("2024-01-02T00:00:00",{ editable = True, edition = Edition (MFloat 250), fromBatch = True, indexCol = "", indexRow = "", override = False, raw = Just "250", value = MFloat (Just 200) })
                    ,("2024-01-03T00:00:00",{ editable = True, edition = Deletion, fromBatch = True, indexCol = "", indexRow = "", override = False, raw = Nothing, value = MFloat (Just 300) })
                    ,("2024-01-04T00:00:00",{ editable = True, edition = Edition (MFloat 400), fromBatch = True, indexCol = "test-series", indexRow = "2024-01-04T00:00:00", override = False, raw = Just "400", value = MFloat Nothing })
                    ,("2024-01-05T00:00:00",{ editable = True, edition = NoEdition, fromBatch = False, indexCol = "", indexRow = "", override = False, raw = Just "500", value = MFloat (Just 500) })
                    ,("2024-01-06T00:00:00",{ editable = True, edition = Deletion, fromBatch = True, indexCol = "test-series", indexRow = "2024-01-06T00:00:00", override = False, raw = Nothing, value = MFloat Nothing })
                    ]
            in
            Expect.equal expectedEntries resultEntries


-- Test data for mergeData
createTestComponents : List Component
createTestComponents =
    let
        -- Component 1: Primary type with float values - use BaseSupervision with override=False, then transform to Complex
        comp1BaseData = Dict.fromList
            [ ("2024-01-01T00:00:00", { value = Just 20.5, override = False })
            , ("2024-01-02T00:00:00", { value = Just 22.1, override = False })
            , ("2024-01-03T00:00:00", { value = Just 19.8, override = False })
            ]
        comp1Data = Dict.map (\_ baseSupervision -> baseToEntry baseSupervision) comp1BaseData

        -- Component 2: Formula type with some overlapping dates as Scalar
        comp2Data = Dict.fromList
            [ ("2024-01-02T00:00:00", Scalar (MFloat (Just 65.0)))
            , ("2024-01-03T00:00:00", Scalar (MFloat (Just 70.2)))
            , ("2024-01-04T00:00:00", Scalar (MFloat (Just 68.5)))
            ]

        -- Component 3: Primary type with string values - use BaseSupervisionString with override=False, then transform to Complex
        comp3BaseData = Dict.fromList
            [ ("2024-01-01T00:00:00", { value = Just "sunny", override = False })
            , ("2024-01-04T00:00:00", { value = Just "cloudy", override = False })
            , ("2024-01-05T00:00:00", { value = Just "rainy", override = False })
            ]
        comp3Data = Dict.map (\_ baseSupervisionString -> baseToEntryString baseSupervisionString) comp3BaseData

        -- Component 4: Auto type with float values as Scalar
        comp4Data = Dict.fromList
            [ ("2024-01-02T00:00:00", Scalar (MFloat (Just 1.5)))
            , ("2024-01-03T00:00:00", Scalar (MFloat (Just 2.8)))
            , ("2024-01-05T00:00:00", Scalar (MFloat (Just 3.2)))
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
      , cType = Primary
      , data = comp3Data
      , tzaware = False
      , status = CompLoaded
      }
    , { name = "pressure"
      , cType = Auto
      , data = comp4Data
      , tzaware = False
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
        Cell entry ->
            case entry.edition of
                NoEdition ->
                    -- Use the original value when no edition
                    case entry.value of
                        MFloat mf -> Maybe.withDefault "" (Maybe.map String.fromFloat mf)
                        MString ms -> Maybe.withDefault "" ms
                Edition scalarType ->
                    -- Use the explicit edited value with edition marker
                    let editedValue = case scalarType of
                            MFloat f -> String.fromFloat f
                            MString s -> s
                    in "*" ++ editedValue ++ "*"  -- Mark editions with asterisks
                Deletion -> "~DEL~"  -- Clear deletion marker
                Error errorMsg -> "[ERROR: " ++ errorMsg ++ "]"  -- Show error messages explicitly


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
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]  -- Header row
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]        -- Row 1: has temp and weather
                    , [ "2024-01-02T00:00:00", "22.1", "65", "", "1.5" ]        -- Row 2: has temp, humidity, and pressure
                    , [ "2024-01-03T00:00:00", "19.8", "70.2", "", "2.8" ]      -- Row 3: has temp, humidity, and pressure
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]       -- Row 4: has humidity and weather
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]         -- Row 5: has weather and pressure
                    ]
            in
            Expect.all
                [ \_ -> Expect.equal expectedResult stringResult
                ]
                ()


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
                    , ((-1, 2), "weather (Primary)")
                    , ((-1, 3), "pressure (Auto)")

                    , ((0, -1), "2024-01-01T00:00:00")       -- Row 1: 2024-01-01
                    , ((0, 0), "20.5")
                    , ((0, 1), "")
                    , ((0, 2), "sunny")
                    , ((0, 3), "")

                    , ((1, -1), "2024-01-02T00:00:00")       -- Row 2: 2024-01-02
                    , ((1, 0), "22.1")
                    , ((1, 1), "65")
                    , ((1, 2), "")
                    , ((1, 3), "1.5")

                    , ((2, -1), "2024-01-03T00:00:00")       -- Row 3: 2024-01-03
                    , ((2, 0), "19.8")
                    , ((2, 1), "70.2")
                    , ((2, 2), "")
                    , ((2, 3), "2.8")

                    , ((3, -1), "2024-01-04T00:00:00")       -- Row 4: 2024-01-04
                    , ((3, 0), "")
                    , ((3, 1), "68.5")
                    , ((3, 2), "cloudy")
                    , ((3, 3), "")

                    , ((4, -1), "2024-01-05T00:00:00")       -- Row 5: 2024-01-05
                    , ((4, 0), "")
                    , ((4, 1), "")
                    , ((4, 2), "rainy")
                    , ((4, 3), "3.2")
                    ]

                allKeys = Dict.keys cartesianResult |> List.sort

                -- Test the reverse operation: convert expectedCoordinates back to grid using dictToGrid
                expectedCoordinatesDict = Dict.fromList (List.map (\((row, col), value) -> ((row, col), value)) expectedCoordinates)
                gridFromDict = dictToGrid expectedCoordinatesDict
                expectedGridFromDict =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "22.1", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "19.8", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]
            in
            Expect.all
                [ \_ -> Expect.equal 30 (Dict.size cartesianResult)  -- Now 6 rows x 5 columns = 30 cells
                , \_ -> Expect.equal (List.map Tuple.first expectedCoordinates) allKeys
                , \_ -> Expect.equal expectedCoordinates coordinateValuePairs
                , \_ -> Expect.equal expectedGridFromDict gridFromDict
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


testPasteRectangle : Test
testPasteRectangle =
    test "pasteRectangle merges patch data into base grid at different corners and data types" <|
        \_ ->
            let
                -- Create base data as List (List Stuff)
                components = createTestComponents
                baseGrid = mergeData components

                -- Test Case 1: Numeric data at corner (1,1)
                pastedData1 = "1\t2\n3\t4"
                parsedData1 = parsePasted pastedData1 False
                patchDict1 = cartesianDataRec parsedData1 [] 0 0 0 Dict.empty
                baseDict = cartesianData baseGrid
                result1 = pasteRectangle True baseDict patchDict1 (1, 1)
                grid1 = dictToGrid result1 |> List.map (List.map stuffToString)
                expected1 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "22.1", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "19.8", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

                -- Test Case 2: Numeric data at corner (1,0)
                result2 = pasteRectangle True baseDict patchDict1 (1, 0)
                grid2 = dictToGrid result2 |> List.map (List.map stuffToString)
                expected2 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "*1*", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "*3*", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

                -- Test Case 3: Numeric data at corner (1,-1)
                result3 = pasteRectangle True baseDict patchDict1 (1, -1)
                grid3 = dictToGrid result3 |> List.map (List.map stuffToString)
                expected3 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "*2*", "65", "", "1.5" ]           -- Ok/pb formula
                    , [ "2024-01-03T00:00:00", "*4*", "70.2", "", "2.8" ]           -- Ok/pb formula
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

                -- Test Case 4: String data at corner (1,1)
                pastedData4 = "a\tb\nc\td"
                parsedData4 = parsePasted pastedData4 True
                patchDict4 = cartesianDataRec parsedData4 [] 0 0 0 Dict.empty
                result4 = pasteRectangle True baseDict patchDict4 (1, 1)
                grid4 = dictToGrid result4 |> List.map (List.map stuffToString)
                expected4 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "22.1", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "19.8", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

                -- Test Case 5: String data at corner (1,0)
                result5 = pasteRectangle True baseDict patchDict4 (1, 0)
                grid5 = dictToGrid result5 |> List.map (List.map stuffToString)
                expected5 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "[ERROR: a]", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "[ERROR: c]", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

                -- Test Case 6: String data at corner (1,-1)
                result6 = pasteRectangle True baseDict patchDict4 (1, -1)
                grid6 = dictToGrid result6 |> List.map (List.map stuffToString)
                expected6 =
                    [ [ "Dates (Primary)", "temperature (Primary)", "humidity (Formula)", "weather (Primary)", "pressure (Auto)" ]
                    , [ "2024-01-01T00:00:00", "20.5", "", "sunny", "" ]
                    , [ "2024-01-02T00:00:00", "[ERROR: b]", "65", "", "1.5" ]
                    , [ "2024-01-03T00:00:00", "[ERROR: d]", "70.2", "", "2.8" ]
                    , [ "2024-01-04T00:00:00", "", "68.5", "cloudy", "" ]
                    , [ "2024-01-05T00:00:00", "", "", "rainy", "3.2" ]
                    ]

            in
            Expect.all
                [ \_ -> Expect.equal expected1 grid1
                , \_ -> Expect.equal expected2 grid2
                , \_ -> Expect.equal expected3 grid3
                , \_ -> Expect.equal expected4 grid4
                , \_ -> Expect.equal expected5 grid5
                , \_ -> Expect.equal expected6 grid6
                ]
                ()
