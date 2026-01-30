module CreateEvent exposing
    ( CreateEventFormData
    , RsvpConfigSelection(..)
    , combineDateTime
    , encodeEvent
    , isEndTimeAfterStartTime
    , isFormValid
    , isValidUrl
    , validateEndTimeMessage
    , validateExternalUrlMessage
    )

{-| Module for creating new events

This module handles:

  - Event creation form state
  - Form validation logic
  - JSON encoding for Supabase API

-}

import Json.Encode


{-| Form data for creating a new event
All fields are strings to match HTML input values
-}
type alias CreateEventFormData =
    { title : String
    , description : String
    , startDate : String -- YYYY-MM-DD from date input
    , startTime : String -- HH:MM from time input
    , endDate : String -- YYYY-MM-DD from date input
    , endTime : String -- HH:MM from time input
    , location : String
    , rsvpConfig : RsvpConfigSelection
    , externalRsvpUrl : String -- Only used when rsvpConfig = ExternalRsvpSelection
    }


{-| Simplified RSVP config enum for form selection
This is different from the domain RsvpConfig type - it's just for the form dropdown
-}
type RsvpConfigSelection
    = NoAttendanceSelection
    | NoRsvpSelection
    | WithRsvpSelection
    | ExternalRsvpSelection


{-| Validate that a URL starts with http:// or https://
-}
isValidUrl : String -> Bool
isValidUrl url =
    String.startsWith "http://" url || String.startsWith "https://" url


{-| Validate external URL and return error message if invalid

Returns Nothing if:

  - URL is empty (no error shown for empty fields)
  - URL is valid (starts with http:// or https://)

Returns Just "error message" if:

  - URL has content but doesn't start with http:// or https://

-}
validateExternalUrlMessage : String -> Maybe String
validateExternalUrlMessage url =
    if String.isEmpty url then
        Nothing

    else if isValidUrl url then
        Nothing

    else
        Just "URL must start with http:// or https://"


{-| Validate end time is after start time and return error message if invalid

Returns Nothing if:

  - Any date/time field is empty (no error shown for incomplete fields)
  - End datetime is after start datetime (valid)

Returns Just "error message" if:

  - All fields are filled but end is before or equal to start

-}
validateEndTimeMessage : String -> String -> String -> String -> Maybe String
validateEndTimeMessage startDate startTime endDate endTime =
    -- Only validate if all fields are filled
    if String.isEmpty startDate || String.isEmpty startTime || String.isEmpty endDate || String.isEmpty endTime then
        Nothing

    else
        let
            startDateTime =
                combineDateTime startDate startTime

            endDateTime =
                combineDateTime endDate endTime
        in
        if isEndTimeAfterStartTime startDateTime endDateTime then
            Nothing

        else
            Just "End time must be after start time"


{-| Combine date (YYYY-MM-DD) and time (HH:MM) into ISO8601 format (YYYY-MM-DDTHH:MM)
-}
combineDateTime : String -> String -> String
combineDateTime date time =
    date ++ "T" ++ time


{-| Validate that end datetime is after start datetime
We can use simple string comparison because ISO8601 format is lexicographically ordered
-}
isEndTimeAfterStartTime : String -> String -> Bool
isEndTimeAfterStartTime startDateTime endDateTime =
    endDateTime > startDateTime


{-| Check if the form is valid and ready to submit

Rules:

  - All text fields must be non-empty
  - All date and time fields must be non-empty
  - End datetime must be after start datetime
  - If RSVP config is ExternalRsvp, the URL must be non-empty and valid

-}
isFormValid : CreateEventFormData -> Bool
isFormValid formData =
    let
        -- Check all required text fields are non-empty
        hasRequiredFields =
            not (String.isEmpty formData.title)
                && not (String.isEmpty formData.description)
                && not (String.isEmpty formData.startDate)
                && not (String.isEmpty formData.startTime)
                && not (String.isEmpty formData.endDate)
                && not (String.isEmpty formData.endTime)
                && not (String.isEmpty formData.location)

        -- Combine date and time fields into ISO8601 format for validation
        startDateTime =
            combineDateTime formData.startDate formData.startTime

        endDateTime =
            combineDateTime formData.endDate formData.endTime

        -- Check dates are in correct order
        datesValid =
            isEndTimeAfterStartTime startDateTime endDateTime

        -- Check external URL if needed
        externalUrlValid =
            case formData.rsvpConfig of
                ExternalRsvpSelection ->
                    not (String.isEmpty formData.externalRsvpUrl)
                        && isValidUrl formData.externalRsvpUrl

                _ ->
                    -- For other RSVP configs, we don't care about the external URL
                    True
    in
    hasRequiredFields && datesValid && externalUrlValid


{-| Encode form data to JSON for POST request to Supabase

The JSON structure matches the events table schema:

    { "title": "...",
      "description": "...",
      "start_time": "2026-01-30T10:00:00",
      "end_time": "2026-01-30T14:00:00",
      "location": "...",
      "image_url": null,
      "rsvp_type": "no_rsvp",
      "external_rsvp_url": "..." // only if rsvp_type is external_rsvp
    }

-}
encodeEvent : CreateEventFormData -> Json.Encode.Value
encodeEvent formData =
    let
        -- Convert form selection to database enum string
        rsvpTypeString =
            case formData.rsvpConfig of
                NoAttendanceSelection ->
                    "no_attendance"

                NoRsvpSelection ->
                    "no_rsvp"

                WithRsvpSelection ->
                    "with_rsvp"

                ExternalRsvpSelection ->
                    "external_rsvp"

        -- Combine date and time fields into ISO8601 format
        startDateTime =
            combineDateTime formData.startDate formData.startTime

        endDateTime =
            combineDateTime formData.endDate formData.endTime

        -- Base fields that are always present
        baseFields =
            [ ( "title", Json.Encode.string formData.title )
            , ( "description", Json.Encode.string formData.description )
            , ( "start_time", Json.Encode.string startDateTime )
            , ( "end_time", Json.Encode.string endDateTime )
            , ( "location", Json.Encode.string formData.location )
            , ( "image_url", Json.Encode.null )
            , ( "rsvp_type", Json.Encode.string rsvpTypeString )
            ]

        -- Add external_rsvp_url only if rsvp_type is external_rsvp
        allFields =
            case formData.rsvpConfig of
                ExternalRsvpSelection ->
                    baseFields
                        ++ [ ( "external_rsvp_url", Json.Encode.string formData.externalRsvpUrl ) ]

                _ ->
                    baseFields
    in
    Json.Encode.object allFields
