module EdiTableSuite exposing
    ( testPatchCurrent
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
