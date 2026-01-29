module EventTest exposing (..)

import Event
import Rsvp exposing (RsvpConfig(..))
import Expect
import Fuzz exposing (..)
import Fuzzers exposing (..)
import Iso8601
import Json.Decode
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
                            |> List.all (\( first, second ) -> first.startTime <= second.startTime)
                in
                Expect.equal True isOrdered
        , test "upcomingEvents filters to events ending today or later" <|
            \_ ->
                let
                    now =
                        Iso8601.toTime "2025-06-15T18:00:00"
                            -- 6pm on June 15
                            |> Result.withDefault (Time.millisToPosix 0)

                    -- Ended yesterday - should be filtered out
                    yesterdayEvent =
                        { id = "test-1"
                        , title = "Yesterday"
                        , description = ""
                        , location = ""
                        , startTime = "2025-06-14T10:00:00"
                        , endTime = "2025-06-14T15:00:00" -- ended June 14
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Ends today (but already finished) - should be included
                    todayEvent =
                        { id = "test-2"
                        , title = "Today"
                        , description = ""
                        , location = ""
                        , startTime = "2025-06-15T10:00:00"
                        , endTime = "2025-06-15T14:00:00" -- ended at 2pm, but still "today"
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Multi-day event ending tomorrow - should be included
                    multiDayEvent =
                        { id = "test-3"
                        , title = "Multi"
                        , description = ""
                        , location = ""
                        , startTime = "2025-06-14T10:00:00" -- started yesterday
                        , endTime = "2025-06-16T15:00:00" -- ends tomorrow
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    -- Future event - should be included
                    futureEvent =
                        { id = "test-4"
                        , title = "Future"
                        , description = ""
                        , location = ""
                        , startTime = "2025-06-20T10:00:00" -- starts June 20
                        , endTime = "2025-06-20T15:00:00" -- ends June 20
                        , imageUrl = Nothing
                        , rsvp = NoRsvp
                        }

                    allEvents =
                        [ yesterdayEvent, todayEvent, multiDayEvent, futureEvent ]

                    result =
                        Event.upcomingEvents now allEvents
                in
                result
                    |> List.map .title
                    |> Expect.equal [ "Today", "Multi", "Future" ]
        , describe "Short Date formatting"
            [ test "formats early date in month with leading zero" <|
                \_ ->
                    let
                        datetime =
                            "2025-01-03T10:00:00"
                    in
                    Expect.equal "03 JAN" (Event.formatDateShort datetime)
            , test "formats double digit date" <|
                \_ ->
                    let
                        datetime =
                            "2025-12-19T15:00:00"
                    in
                    Expect.equal "19 DEC"
                        (Event.formatDateShort datetime)
            , test "formats datetimes with minute and second components" <|
                \_ ->
                    let
                        datetime =
                            "2025-03-25T12:30:30"
                    in
                    Expect.equal "25 MAR"
                        (Event.formatDateShort datetime)
            ]
        , describe "Decoding event from JSON"
            [ test "decodes a valid event from JSON with NoRsvp" <|
                \_ ->
                    let
                        json =
                            """
              {
                  "id": "123e4567-e89b-12d3-a456-426614174000",
                  "title": "New Years Eve",
                  "description": "Celebration for the new year!",
                  "start_time": "2026-12-31T23:59:59",
                  "end_time": "2026-01-01T09:00:00",
                  "location": "Fryerstown School Hall",
                  "image_url": null,
                  "rsvp_type": "no_rsvp",
                  "external_rsvp_url": null,
                  "created_at": "2026-01-29T01:50:16+00:00",
                  "updated_at": null
              }
              """

                        expected =
                            { id = "123e4567-e89b-12d3-a456-426614174000"
                            , title = "New Years Eve"
                            , description = "Celebration for the new year!"
                            , startTime = "2026-12-31T23:59:59"
                            , endTime = "2026-01-01T09:00:00"
                            , location = "Fryerstown School Hall"
                            , imageUrl = Nothing
                            , rsvp = NoRsvp
                            }
                    in
                    Json.Decode.decodeString Event.eventDecoder json
                        |> Expect.equal (Ok expected)
            , test "decodes a valid event from JSON with NoAttendance" <|
                \_ ->
                    let
                        json =
                            """
              {
                  "id": "123e4567-e89b-12d3-a456-426614174000",
                  "title": "New Years Eve",
                  "description": "Celebration for the new year!",
                  "start_time": "2026-12-31T23:59:59",
                  "end_time": "2026-01-01T09:00:00",
                  "location": "Fryerstown School Hall",
                  "image_url": null,
                  "rsvp_type": "no_attendance",
                  "external_rsvp_url": null,
                  "created_at": "2026-01-29T01:50:16+00:00",
                  "updated_at": null
              }
              """

                        expected =
                            { id = "123e4567-e89b-12d3-a456-426614174000"
                            , title = "New Years Eve"
                            , description = "Celebration for the new year!"
                            , startTime = "2026-12-31T23:59:59"
                            , endTime = "2026-01-01T09:00:00"
                            , location = "Fryerstown School Hall"
                            , imageUrl = Nothing
                            , rsvp = NoAttendance
                            }
                    in
                    Json.Decode.decodeString Event.eventDecoder json
                        |> Expect.equal (Ok expected)
            , test "decodes a valid event from JSON with ExternalRsvp" <|
                \_ ->
                    let
                        json =
                            """
              {
                  "id": "123e4567-e89b-12d3-a456-426614174000",
                  "title": "New Years Eve",
                  "description": "Celebration for the new year!",
                  "start_time": "2026-12-31T23:59:59",
                  "end_time": "2026-01-01T09:00:00",
                  "location": "Fryerstown School Hall",
                  "image_url": null,
                  "rsvp_type": "external_rsvp",
                  "external_rsvp_url": "http://eventsrus.com/event-1",
                  "created_at": "2026-01-29T01:50:16+00:00",
                  "updated_at": null
              }
              """

                        expected =
                            { id = "123e4567-e89b-12d3-a456-426614174000"
                            , title = "New Years Eve"
                            , description = "Celebration for the new year!"
                            , startTime = "2026-12-31T23:59:59"
                            , endTime = "2026-01-01T09:00:00"
                            , location = "Fryerstown School Hall"
                            , imageUrl = Nothing
                            , rsvp = ExternalRsvp "http://eventsrus.com/event-1"
                            }
                    in
                    Json.Decode.decodeString Event.eventDecoder json
                        |> Expect.equal (Ok expected)
            , test "decodes a valid event from JSON with WithRsvp" <|
                \_ ->
                    let
                        json =
                            """
              {
                  "id": "123e4567-e89b-12d3-a456-426614174000",
                  "title": "New Years Eve",
                  "description": "Celebration for the new year!",
                  "start_time": "2026-12-31T23:59:59",
                  "end_time": "2026-01-01T09:00:00",
                  "location": "Fryerstown School Hall",
                  "image_url": null,
                  "rsvp_type": "with_rsvp",
                  "external_rsvp_url": null,
                  "created_at": "2026-01-29T01:50:16+00:00",
                  "updated_at": null
              }
              """

                        expected =
                            { id = "123e4567-e89b-12d3-a456-426614174000"
                            , title = "New Years Eve"
                            , description = "Celebration for the new year!"
                            , startTime = "2026-12-31T23:59:59"
                            , endTime = "2026-01-01T09:00:00"
                            , location = "Fryerstown School Hall"
                            , imageUrl = Nothing
                            , rsvp = WithRsvp []
                            }
                    in
                    Json.Decode.decodeString Event.eventDecoder json
                        |> Expect.equal (Ok expected)
            ]
        , describe "Short Time formatting"
            [ test "formats early time in day without minutes" <|
                \_ ->
                    let
                        start =
                            "2025-01-03T07:00:00"

                        end =
                            "2025-01-03T09:00:00"
                    in
                    Expect.equal "7am until 9am" (Event.formatStartEndShort start end)
            , test "formats early time in day with minutes" <|
                \_ ->
                    let
                        start =
                            "2025-12-19T07:30:00"

                        end =
                            "2025-12-19T09:30:00"
                    in
                    Expect.equal "7:30am until 9:30am"
                        (Event.formatStartEndShort start end)
            , test "formats early start afternoon end" <|
                \_ ->
                    let
                        start =
                            "2025-03-25T09:00:00"

                        end =
                            "2025-03-25T15:30:00"
                    in
                    Expect.equal "9am until 3:30pm"
                        (Event.formatStartEndShort start end)
            , test "formats evening time until late" <|
                \_ ->
                    let
                        start =
                            "2025-03-25T19:00:00"

                        end =
                            "2025-03-25T23:59:59"
                    in
                    Expect.equal "7pm until late"
                        (Event.formatStartEndShort start end)
            , test "gracefully handles start = end" <|
                \_ ->
                    let
                        start =
                            "2025-03-25T19:00:00"

                        end =
                            "2025-03-25T19:00:00"
                    in
                    Expect.equal "7pm"
                        (Event.formatStartEndShort start end)
            , test "gracefully handles invalid times" <|
                \_ ->
                    let
                        start =
                            "mid-morning"

                        end =
                            "after lunch"
                    in
                    Expect.equal "mid-morning until after lunch"
                        (Event.formatStartEndShort start end)
            ]
        ]
