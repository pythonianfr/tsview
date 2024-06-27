module DateIntervalSuite exposing (..)

import Expect as E
import Test as T
import Dateinterval as D exposing (..)


dateRangeSuite : T.Test
dateRangeSuite = T.concat
    [ T.test
        """
        Produce a list of 3 points, start at the initial
        date 2024-01-01, each point with an increment of 1h
        """ <|
            \_ -> E.equal
                    (Just
                        ["2024-01-01T00:00:00.000Z"
                        ,"2024-01-01T01:00:00.000Z"
                        ,"2024-01-01T02:00:00.000Z"
                        ]
                    )
                    (D.dateRange "2024-01-01" 3600000 3)
    , T.test "Return Nothing in case the date is not ISO compatible"
         <|
            \_ -> E.equal
                Nothing
                (D.dateRange "2024" 3600000 3)
    ]