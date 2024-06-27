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
                    [ (days, String.fromInt days ++ " days")
                    , (hours, String.fromInt hours ++ " hours")
                    , (minutes, String.fromInt minutes ++ " minutes")
                    , (seconds, String.fromInt seconds ++ " seconds")
                    , (milliseconds, String.fromInt milliseconds ++ " milliseconds")
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