module DateIntervalSuite exposing (..)

import Expect as E
import Test as T
import Dateinterval as D exposing (..)


dateRangeSuite : T.Test
dateRangeSuite =
    T.concat
        [ T.test
              """
               Produce a list of 3 points, start at the initial
               date 2024-01-01, each point with an increment of 1h
               """ <|
              \_ -> E.equal
              ( Just
                   [ "2024-01-01T00:00:00.000Z"
                   , "2024-01-01T01:00:00.000Z"
                   , "2024-01-01T02:00:00.000Z"
                   ]
              )
              ( D.dateRange "2024-01-01" 3600000 3 )
        , T.test "Return Nothing in case the date is not ISO compatible"
            <|
            \_ -> E.equal
                  Nothing
                  ( D.dateRange "2024" 3600000 3 )
        ]


dateDiffSuite : T.Test
dateDiffSuite =
    T.concat
        [ T.test "From a list of 3 dates, return the list of 2 intervals" <|
              \_ -> E.equal
              ( Just [ 3600000, 3600000 ] )
              ( D.dateDiff
                    [ "2024-01-01T00:00:00.000Z"
                    , "2024-01-01T01:00:00.000Z"
                    , "2024-01-01T02:00:00.000Z"
                    ]
              )
        , T.test "Returns Nothing if one of the dates is not in ISO format"
            <|
            \_ -> E.equal
                  Nothing
                  ( D.dateDiff
                        [ "2024"
                        , "2024-01-01T01:00:00.000Z"
                        , "2024-01-01T02:00:00.000Z"
                        ]
                  )
        , T.test "Returns Nothing if the list is empty"
            <|
            \_ -> E.equal
                  Nothing
                  ( D.dateDiff [] )
        , T.test "Returns Nothing if the length of the list is 1"
         <|
         \_ -> E.equal
               Nothing
               ( D.dateDiff [ "2024-01-01T01:00:00.000Z" ] )
        ]


medianValueSuite : T.Test
medianValueSuite =
    T.concat
        [ T.test
              """
               Calculate the median of a list of date intervals.
               In this example, the median is 17 days.
               """ <|
              \_ -> E.equal
              ( Just "17 days" )
              ( D.medianValue
                    [ "2024-01-16"
                    , "2024-02-05"
                    , "2024-03-09"
                    , "2024-03-21"
                    , "2024-04-01"
                    , "2024-04-18"
                    , "2024-04-26"
                    , "2024-05-07"
                    , "2024-06-01"
                    , "2024-07-09"
                    , "2024-08-05"
                    , "2024-08-17"
                    , "2024-09-03"
                    , "2024-09-15"
                    , "2024-09-22"
                    , "2024-10-04"
                    , "2024-10-13"
                    , "2024-11-02"
                    , "2024-12-05"
                    , "2024-12-23"
                    ]
              )
        ]
