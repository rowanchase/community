module Rsvp exposing (..)

import Json.Decode
import Json.Encode


type alias Url =
    String


type alias PhoneNumber =
    String


type alias EmailAddress =
    String


type alias Rsvp =
    { fullName : String
    , phone : Maybe PhoneNumber
    , email : Maybe EmailAddress
    , adults : Int
    , children : Int
    }


type RsvpConfig
    = NoAttendance
    | NoRsvp
    | WithRsvp (List Rsvp)
    | ExternalRsvp Url


type alias RsvpFormData =
    { fullName : String
    , adults : String
    , children : String
    }


isRsvpFormValid : RsvpFormData -> Bool
isRsvpFormValid formData =
    let
        hasValidName =
            not (String.isEmpty (String.trim formData.fullName))

        hasValidAdults =
            case String.toInt formData.adults of
                Just n ->
                    n >= 0

                Nothing ->
                    False

        hasValidChildren =
            case String.toInt formData.children of
                Just n ->
                    n >= 0

                Nothing ->
                    False
    in
    hasValidName && hasValidAdults && hasValidChildren


encodeRsvp : String -> RsvpFormData -> Json.Encode.Value
encodeRsvp eventId formData =
    Json.Encode.object
        [ ( "event_id", Json.Encode.string eventId )
        , ( "full_name", Json.Encode.string (String.trim formData.fullName) )
        , ( "adults", Json.Encode.int (String.toInt formData.adults |> Maybe.withDefault 1) )
        , ( "children", Json.Encode.int (String.toInt formData.children |> Maybe.withDefault 0) )
        ]


rsvpConfigDecoder : Json.Decode.Decoder RsvpConfig
rsvpConfigDecoder =
    Json.Decode.field "rsvp_type" Json.Decode.string
        |> Json.Decode.andThen rsvpConfigFromString


rsvpConfigFromString : String -> Json.Decode.Decoder RsvpConfig
rsvpConfigFromString rsvpType =
    case rsvpType of
        "no_attendance" ->
            Json.Decode.succeed NoAttendance

        "no_rsvp" ->
            Json.Decode.succeed NoRsvp

        "external_rsvp" ->
            Json.Decode.map ExternalRsvp
                (Json.Decode.field "external_rsvp_url" Json.Decode.string)

        "with_rsvp" ->
            Json.Decode.succeed (WithRsvp [])

        _ ->
            Json.Decode.fail ("Unknown rsvp_type: " ++ rsvpType)
