module TimeSuite exposing (..)

import Expect exposing (Expectation)
import Test as T
import TimeUtil


suiteUtcToLocal : T.Test
suiteUtcToLocal =
    T.concat
    [ T.test "utcToLocal with valid timezone - Europe/Paris winter"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-01-15T14:30:00Z")
                "2024-01-15T15:30:00+0100"
        )
    , T.test "utcToLocal with valid timezone - Europe/Paris summer"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-07-15T14:30:00Z")
                "2024-07-15T16:30:00+0200"
        )
    , T.test "utcToLocal with valid timezone - Europe/Berlin winter"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Berlin" "2024-01-15T14:30:00Z")
                "2024-01-15T15:30:00+0100"
        )
    , T.test "utcToLocal with valid timezone - Europe/Berlin summer"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Berlin" "2024-07-15T14:30:00Z")
                "2024-07-15T16:30:00+0200"
        )
    , T.test "utcToLocal with CET timezone (case insensitive)"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "cet" "2024-01-15T14:30:00Z")
                "2024-01-15T15:30:00+0100"
        )
    , T.test "utcToLocal with CET timezone (uppercase)"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "CET" "2024-01-15T14:30:00Z")
                "2024-01-15T15:30:00+0100"
        )
    , T.test "utcToLocal with invalid timezone"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Invalid/Timezone" "2024-01-15T14:30:00Z")
                "Wrong timezone name"
        )
    , T.test "utcToLocal with invalid ISO date"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "invalid-date")
                "iso8601-error"
        )
    , T.test "utcToLocal with midnight UTC"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-01-15T00:00:00Z")
                "2024-01-15T01:00:00+0100"
        )
    , T.test "utcToLocal with near midnight crossing day boundary"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-01-14T23:30:00Z")
                "2024-01-15T00:30:00+0100"
        )
    , T.test "utcToLocal with different timezone - America/New_York winter"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "America/New_York" "2024-01-15T14:30:00Z")
                "2024-01-15T09:30:00-0500"
        )
    , T.test "utcToLocal with different timezone - America/New_York summer"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "America/New_York" "2024-07-15T14:30:00Z")
                "2024-07-15T10:30:00-0400"
        )
    , T.test "utcToLocal with Asia/Tokyo (no DST)"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Asia/Tokyo" "2024-01-15T14:30:00Z")
                "2024-01-15T23:30:00+0900"
        )
    , T.test "utcToLocal with leap year date"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-02-29T12:00:00Z")
                "2024-02-29T13:00:00+0100"
        )
    , T.test "utcToLocal with DST transition date spring"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-04-01T01:00:00Z")
                "2024-04-01T03:00:00+0200"
        )
    , T.test "utcToLocal with DST transition date fall"
        (\_ -> Expect.equal
                (TimeUtil.utcToLocal "Europe/Paris" "2024-11-01T01:00:00Z")
                "2024-11-01T02:00:00+0100"
        )
    ]


suiteLocalize : T.Test
suiteLocalize =
    T.concat
    [ T.test "localize with tzaware=False returns original string"
        (\_ -> Expect.equal
                (TimeUtil.localize "Europe/Paris" False "2024-01-15T14:30:00")
                "2024-01-15T14:30:00"
        )
    , T.test "localize with tzaware=True and UTC timezone"
        (\_ -> Expect.equal
                (TimeUtil.localize "utc" True "2024-01-15T14:30:00Z")
                "2024-01-15T14:30:00+00:00"
        )
    , T.test "localize with tzaware=True and non-UTC timezone"
        (\_ -> Expect.equal
                (TimeUtil.localize "Europe/Paris" True "2024-01-15T14:30:00Z")
                "2024-01-15T15:30:00+0100"
        )
    ]
