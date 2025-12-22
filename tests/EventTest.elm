module EventTest exposing (..)

import Event exposing (RsvpConfig(..))
import Expect
import Fuzz exposing (..)
import Fuzzers exposing (..)
import Iso8601
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Event"
        [ fuzz (Fuzz.list Fuzzers.event) "sorting is idempotent" <|
            \randomEvents ->
                let
                    sortedOnce =
                        Event.sortByStartTime randomEvents

                    sortedTwice =
                        Event.sortByStartTime sortedOnce
                in
                Expect.equal sortedOnce sortedTwice
        , fuzz (Fuzz.list Fuzzers.event) "sortByStartTime produces chronologically ordered events" <|
            \randomEvents ->
                let
                    sorted =
                        Event.sortByStartTime randomEvents

                    isOrdered =
                        List.map2 Tuple.pair sorted (List.drop 1 sorted)
                            |> List.all (\( first, second ) -> first.start <= second.start)
                in
                Expect.equal True isOrdered
        , test "upcomingEvents filters to events ending today or later" <|
            \_ ->
                let
                    zone =
                        Time.utc

                    now =
                        Iso8601.toTime "2025-06-15T18:00:00"
                            -- 6pm on June 15
                            |> Result.withDefault (Time.millisToPosix 0)

                    -- Ended yesterday - should be filtered out
                    yesterdayEvent =
                        { title = "Yesterday"
                        , description = ""
                        , location = ""
                        , start = "2025-06-14T10:00:00"
                        , end = "2025-06-14T15:00:00" -- ended June 14
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Ends today (but already finished) - should be included
                    todayEvent =
                        { title = "Today"
                        , description = ""
                        , location = ""
                        , start = "2025-06-15T10:00:00"
                        , end = "2025-06-15T14:00:00" -- ended at 2pm, but still "today"
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Multi-day event ending tomorrow - should be included
                    multiDayEvent =
                        { title = "Multi"
                        , description = ""
                        , location = ""
                        , start = "2025-06-14T10:00:00" -- started yesterday
                        , end = "2025-06-16T15:00:00" -- ends tomorrow
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Future event - should be included
                    futureEvent =
                        { title = "Future"
                        , description = ""
                        , location = ""
                        , start = "2025-06-20T10:00:00" -- starts June 20
                        , end = "2025-06-20T15:00:00" -- ends June 20
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    allEvents =
                        [ yesterdayEvent, todayEvent, multiDayEvent, futureEvent ]

                    result =
                        Event.upcomingEvents zone now allEvents
                in
                result
                    |> List.map .title
                    |> Expect.equal [ "Today", "Multi", "Future" ]
        ]
