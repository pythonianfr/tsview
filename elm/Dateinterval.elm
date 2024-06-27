module Dateinterval exposing (..)
import Iso8601
import Time
import List.Extra as List
import Maybe.Extra as Maybe


dateRange : String -> Int -> Int -> Maybe (List String)
dateRange isoDate delta points =
    case Iso8601.toTime isoDate of
        Ok time ->
            Just (List.range 0 (points - 1)
                    |> List.map (\x -> (Time.posixToMillis time) + x * delta)
                    |> List.map Time.millisToPosix
                    |> List.map Iso8601.fromTime)
        Err _ ->
            Nothing


dateDiff : List String -> Maybe (List Int)
dateDiff listStringDates =
    let
        dateToInt : String -> Maybe Int
        dateToInt isoDate =
            case Iso8601.toTime isoDate of
                Ok time ->
                    Just (Time.posixToMillis time)
                Err _ ->
                    Nothing
    in
    if List.length listStringDates <= 1 then
        Nothing
    else
        listStringDates
            |> List.drop 1
            |> List.zip listStringDates
            |> List.map (\(a, b) -> Maybe.map2 (-) (dateToInt b) (dateToInt a))
            |> Maybe.combine