module CreateEventTest exposing (suite)

import CreateEvent exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Test exposing (..)


{-| Test suite for event creation validation and encoding
-}
suite : Test
suite =
    describe "CreateEvent module"
        [ urlValidationTests
        , dateValidationTests
        , formValidationTests
        , encodingTests
        ]


{-| Tests for URL format validation
-}
urlValidationTests : Test
urlValidationTests =
    describe "isValidUrl"
        [ test "accepts URLs starting with http://" <|
            \_ ->
                isValidUrl "http://example.com"
                    |> Expect.equal True
        , test "accepts URLs starting with https://" <|
            \_ ->
                isValidUrl "https://example.com"
                    |> Expect.equal True
        , test "rejects URLs without protocol" <|
            \_ ->
                isValidUrl "example.com"
                    |> Expect.equal False
        , test "rejects URLs with other protocols" <|
            \_ ->
                isValidUrl "ftp://example.com"
                    |> Expect.equal False
        , test "rejects empty strings" <|
            \_ ->
                isValidUrl ""
                    |> Expect.equal False
        , test "accepts URLs with paths and query parameters" <|
            \_ ->
                isValidUrl "https://example.com/path?query=value"
                    |> Expect.equal True
        ]


{-| Tests for date/time validation and combination
-}
dateValidationTests : Test
dateValidationTests =
    describe "Date and time handling"
        [ describe "combineDateTime"
            [ test "combines date and time correctly" <|
                \_ ->
                    combineDateTime "2026-01-30" "14:00"
                        |> Expect.equal "2026-01-30T14:00"
            , test "works with different times" <|
                \_ ->
                    combineDateTime "2025-12-31" "23:59"
                        |> Expect.equal "2025-12-31T23:59"
            ]
        , describe "isEndTimeAfterStartTime"
            [ test "returns true when end time is after start time" <|
                \_ ->
                    isEndTimeAfterStartTime "2026-01-30T10:00" "2026-01-30T14:00"
                        |> Expect.equal True
            , test "returns false when end time is before start time" <|
                \_ ->
                    isEndTimeAfterStartTime "2026-01-30T14:00" "2026-01-30T10:00"
                        |> Expect.equal False
            , test "returns false when end time equals start time" <|
                \_ ->
                    isEndTimeAfterStartTime "2026-01-30T10:00" "2026-01-30T10:00"
                        |> Expect.equal False
            , test "works across days" <|
                \_ ->
                    isEndTimeAfterStartTime "2026-01-30T23:00" "2026-01-31T02:00"
                        |> Expect.equal True
            , test "works across months" <|
                \_ ->
                    isEndTimeAfterStartTime "2026-01-31T10:00" "2026-02-01T10:00"
                        |> Expect.equal True
            , test "works across years" <|
                \_ ->
                    isEndTimeAfterStartTime "2025-12-31T23:00" "2026-01-01T01:00"
                        |> Expect.equal True
            ]
        ]


{-| Tests for complete form validation
-}
formValidationTests : Test
formValidationTests =
    describe "isFormValid"
        [ test "valid form with NoRsvp returns true" <|
            \_ ->
                let
                    validForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid validForm
                    |> Expect.equal True
        , test "empty title returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = ""
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "empty description returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = ""
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "empty location returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = ""
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "empty start time returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = ""
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "empty end time returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = ""
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "end time before start time returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "14:00"
                        , endDate = "2026-01-30"
                        , endTime = "10:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "ExternalRsvp with valid URL returns true" <|
            \_ ->
                let
                    validForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = ExternalRsvpSelection
                        , externalRsvpUrl = "https://example.com/rsvp"
                        }
                in
                isFormValid validForm
                    |> Expect.equal True
        , test "ExternalRsvp with empty URL returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = ExternalRsvpSelection
                        , externalRsvpUrl = ""
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "ExternalRsvp with invalid URL returns false" <|
            \_ ->
                let
                    invalidForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = ExternalRsvpSelection
                        , externalRsvpUrl = "not-a-valid-url"
                        }
                in
                isFormValid invalidForm
                    |> Expect.equal False
        , test "other RSVP configs ignore external URL validation" <|
            \_ ->
                let
                    validForm =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = WithRsvpSelection
                        , externalRsvpUrl = "invalid-url"
                        }
                in
                isFormValid validForm
                    |> Expect.equal True
        ]


{-| Tests for JSON encoding
-}
encodingTests : Test
encodingTests =
    describe "encodeEvent"
        [ test "encodes NoRsvp correctly" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.map2 Tuple.pair
                                (Json.Decode.field "title" Json.Decode.string)
                                (Json.Decode.field "rsvp_type" Json.Decode.string)
                            )
                            encoded
                in
                decoded
                    |> Expect.equal (Ok ( "Test Event", "no_rsvp" ))
        , test "encodes NoAttendance correctly" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoAttendanceSelection
                        , externalRsvpUrl = ""
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.field "rsvp_type" Json.Decode.string)
                            encoded
                in
                decoded
                    |> Expect.equal (Ok "no_attendance")
        , test "encodes WithRsvp correctly" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = WithRsvpSelection
                        , externalRsvpUrl = ""
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.field "rsvp_type" Json.Decode.string)
                            encoded
                in
                decoded
                    |> Expect.equal (Ok "with_rsvp")
        , test "encodes ExternalRsvp with URL correctly" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = ExternalRsvpSelection
                        , externalRsvpUrl = "https://example.com/rsvp"
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.map2 Tuple.pair
                                (Json.Decode.field "rsvp_type" Json.Decode.string)
                                (Json.Decode.field "external_rsvp_url" Json.Decode.string)
                            )
                            encoded
                in
                decoded
                    |> Expect.equal (Ok ( "external_rsvp", "https://example.com/rsvp" ))
        , test "encodes all required fields" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.map6
                                (\title desc start end loc rsvp ->
                                    { title = title
                                    , description = desc
                                    , startTime = start
                                    , endTime = end
                                    , location = loc
                                    , rsvpType = rsvp
                                    }
                                )
                                (Json.Decode.field "title" Json.Decode.string)
                                (Json.Decode.field "description" Json.Decode.string)
                                (Json.Decode.field "start_time" Json.Decode.string)
                                (Json.Decode.field "end_time" Json.Decode.string)
                                (Json.Decode.field "location" Json.Decode.string)
                                (Json.Decode.field "rsvp_type" Json.Decode.string)
                            )
                            encoded
                in
                decoded
                    |> Expect.equal
                        (Ok
                            { title = "Test Event"
                            , description = "Test description"
                            , startTime = "2026-01-30T10:00"
                            , endTime = "2026-01-30T14:00"
                            , location = "Test Location"
                            , rsvpType = "no_rsvp"
                            }
                        )
        , test "image_url is always null" <|
            \_ ->
                let
                    formData =
                        { title = "Test Event"
                        , description = "Test description"
                        , startDate = "2026-01-30"
                        , startTime = "10:00"
                        , endDate = "2026-01-30"
                        , endTime = "14:00"
                        , location = "Test Location"
                        , rsvpConfig = NoRsvpSelection
                        , externalRsvpUrl = ""
                        }

                    encoded =
                        encodeEvent formData
                            |> Json.Encode.encode 0

                    decoded =
                        Json.Decode.decodeString
                            (Json.Decode.field "image_url" (Json.Decode.null ()))
                            encoded
                in
                decoded
                    |> Expect.equal (Ok ())
        ]
