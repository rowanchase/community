module CreateEvent exposing
    ( CreateEventFormData
    , RsvpConfigSelection(..)
    , encodeEvent
    , isEndTimeAfterStartTime
    , isFormValid
    , isValidUrl
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
    , startTime : String -- ISO8601 from datetime-local input
    , endTime : String -- ISO8601 from datetime-local input
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


{-| Validate that end time is after start time
We can use simple string comparison because ISO8601 format is lexicographically ordered
-}
isEndTimeAfterStartTime : String -> String -> Bool
isEndTimeAfterStartTime startTime endTime =
    endTime > startTime


{-| Check if the form is valid and ready to submit

Rules:

  - All text fields must be non-empty
  - End time must be after start time
  - If RSVP config is ExternalRsvp, the URL must be non-empty and valid

-}
isFormValid : CreateEventFormData -> Bool
isFormValid formData =
    let
        -- Check all required text fields are non-empty
        hasRequiredFields =
            not (String.isEmpty formData.title)
                && not (String.isEmpty formData.description)
                && not (String.isEmpty formData.startTime)
                && not (String.isEmpty formData.endTime)
                && not (String.isEmpty formData.location)

        -- Check dates are in correct order
        datesValid =
            isEndTimeAfterStartTime formData.startTime formData.endTime

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

        -- Base fields that are always present
        baseFields =
            [ ( "title", Json.Encode.string formData.title )
            , ( "description", Json.Encode.string formData.description )
            , ( "start_time", Json.Encode.string formData.startTime )
            , ( "end_time", Json.Encode.string formData.endTime )
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
