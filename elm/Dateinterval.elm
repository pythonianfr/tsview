module Dateinterval exposing (..)
import Iso8601
import Time
import List.Extra as List


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