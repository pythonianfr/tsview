module Dateinterval exposing (..)
import Iso8601
import Time
import List.Extra as List
import Maybe.Extra as Maybe
import List.Statistics as List


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


medianValue : List String -> Maybe String
medianValue listStringDates =
    let
        convertToTimeComponents : Float -> String
        convertToTimeComponents median =
            let
                medianInt = Basics.round median
                totalSeconds = medianInt // 1000
                totalMinutes = totalSeconds // 60
                totalHours = totalMinutes // 60
                days = totalHours // 24
                hours = Basics.modBy 24 totalHours
                minutes = Basics.modBy 60 totalMinutes
                seconds = Basics.modBy 60 totalSeconds
                milliseconds = Basics.modBy 1000 medianInt
                converted =
                    [ (days, String.fromInt days ++ if days <= 1 then " day" else " days")
                    , (hours, String.fromInt hours ++ if hours <= 1 then " hour" else " hours")
                    , (minutes, String.fromInt minutes ++ if minutes <= 1 then " minute" else " minutes")
                    , (seconds, String.fromInt seconds ++ if seconds <= 1 then " second" else " seconds")
                    , (milliseconds, String.fromInt milliseconds ++ if milliseconds <= 1 then " millisecond" else " milliseconds")
                    ]
                        |> List.filter (\(time, _) -> time /= 0)
                        |> List.map (\(_, strTime) -> strTime)
            in if median == 0 then "0" else String.join " " converted
    in
    listStringDates
        |> dateDiff
        |> Maybe.andThen (\times ->
            times
                |> List.sort
                |> List.map Basics.toFloat
                |> List.median
                |> Maybe.map convertToTimeComponents
                )