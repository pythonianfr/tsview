module TimeUtil exposing
    ( localize
    , utcToLocal
    )

import Dict
import Time
import TimeZone
import Iso8601
import Result


-- date conversion

localize: String -> Bool -> String -> String
localize zone tzaware isoString =
    if not tzaware
    then isoString
    else
        if String.toLower zone == "utc"
            then case Iso8601.toTime isoString of
                    Ok utcPosix -> formatAsUtcIso utcPosix
                    Err _ -> "iso8601-error"
            else  utcToLocal zone isoString


utcToLocal : String -> String -> String
utcToLocal zone isoString =
  case Dict.get zone TimeZone.zones of
      Nothing ->
        if String.toLower zone == "cet"
            then  toLocal ( TimeZone.europe__paris () ) isoString
            else "Wrong timezone name"
      Just tzone -> toLocal ( tzone () ) isoString


toLocal : Time.Zone -> String -> String
toLocal tzone isoString =
    case Iso8601.toTime isoString of
          Ok utcPosix ->
              let
                  tZone = tzone
                  -- Get local time parts
                  year = Time.toYear tZone utcPosix
                  month = Time.toMonth tZone utcPosix
                  day = Time.toDay tZone utcPosix
                  hour = Time.toHour tZone utcPosix
                  minute = Time.toMinute tZone utcPosix
                  second = Time.toSecond tZone utcPosix

                  -- Calculate offset
                  offsetMinutes = getTimezoneOffset tZone utcPosix
                  offsetString = formatOffset offsetMinutes
              in
              formatDateTimeWithOffset year month day hour minute second offsetString

          Err _ -> "iso8601-error"


-- Calculate timezone offset in minutes
getTimezoneOffset : Time.Zone -> Time.Posix -> Int
getTimezoneOffset zone posix =
  let
      utcHour = Time.toHour Time.utc posix
      utcMinute = Time.toMinute Time.utc posix
      localHour = Time.toHour zone posix
      localMinute = Time.toMinute zone posix

      utcTotalMinutes = utcHour * 60 + utcMinute
      localTotalMinutes = localHour * 60 + localMinute

      -- Handle day boundary crossing
      rawDiff = localTotalMinutes - utcTotalMinutes
  in
  if rawDiff > 12 * 60 then
      rawDiff - 24 * 60
  else if rawDiff < -12 * 60 then
      rawDiff + 24 * 60
  else
      rawDiff

-- Format offset as +HHMM or -HHMM
formatOffset : Int -> String
formatOffset offsetMinutes =
  let
      sign = if offsetMinutes >= 0 then "+" else "-"
      absMinutes = abs offsetMinutes
      hours = absMinutes // 60
      minutes = modBy 60 absMinutes
  in
  sign
  ++ String.padLeft 2 '0' (String.fromInt hours)
  ++ String.padLeft 2 '0' (String.fromInt minutes)

-- Format date time with offset
formatDateTimeWithOffset : Int -> Time.Month -> Int -> Int -> Int -> Int -> String -> String
formatDateTimeWithOffset year month day hour minute second offset =
  String.fromInt year ++ "-"
  ++ monthToString month ++ "-"
  ++ String.padLeft 2 '0' (String.fromInt day) ++ "T"
  ++ String.padLeft 2 '0' (String.fromInt hour) ++ ":"
  ++ String.padLeft 2 '0' (String.fromInt minute) ++ ":"
  ++ String.padLeft 2 '0' (String.fromInt second)
  ++ offset

monthToString : Time.Month -> String
monthToString month =
  case month of
      Time.Jan -> "01"
      Time.Feb -> "02"
      Time.Mar -> "03"
      Time.Apr -> "04"
      Time.May -> "05"
      Time.Jun -> "06"
      Time.Jul -> "07"
      Time.Aug -> "08"
      Time.Sep -> "09"
      Time.Oct -> "10"
      Time.Nov -> "11"
      Time.Dec -> "12"

formatAsUtcIso : Time.Posix -> String
formatAsUtcIso posix =
  let
      year = Time.toYear Time.utc posix
      month = Time.toMonth Time.utc posix
      day = Time.toDay Time.utc posix
      hour = Time.toHour Time.utc posix
      minute = Time.toMinute Time.utc posix
      second = Time.toSecond Time.utc posix
  in
  String.fromInt year ++ "-"
  ++ monthToString month ++ "-"
  ++ String.padLeft 2 '0' (String.fromInt day) ++ "T"
  ++ String.padLeft 2 '0' (String.fromInt hour) ++ ":"
  ++ String.padLeft 2 '0' (String.fromInt minute) ++ ":"
  ++ String.padLeft 2 '0' (String.fromInt second) ++ "+00:00"
