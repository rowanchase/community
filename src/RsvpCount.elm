module RsvpCount exposing
    ( RsvpCount
    , RsvpCounts
    , formatCountMessage
    , getCountForEvent
    , rsvpCountDecoder
    , rsvpCountsDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


{-| Represents the aggregated RSVP count for a single event
-}
type alias RsvpCount =
    { eventId : String
    , rsvpCount : Int
    , adultCount : Int
    , childrenCount : Int
    , peopleCount : Int
    }


{-| A dictionary of RSVP counts keyed by event ID
-}
type alias RsvpCounts =
    Dict String RsvpCount


{-| Decoder for a single RSVP count from the database view
-}
rsvpCountDecoder : Decoder RsvpCount
rsvpCountDecoder =
    Decode.map5 RsvpCount
        (Decode.field "event_id" Decode.string)
        (Decode.field "rsvp_count" Decode.int)
        (Decode.field "adult_count" Decode.int)
        (Decode.field "children_count" Decode.int)
        (Decode.field "people_count" Decode.int)


{-| Decoder for a list of RSVP counts, converting to a Dict keyed by event\_id
-}
rsvpCountsDecoder : Decoder RsvpCounts
rsvpCountsDecoder =
    Decode.list rsvpCountDecoder
        |> Decode.map (List.map (\count -> ( count.eventId, count )))
        |> Decode.map Dict.fromList


{-| Get the people count for a specific event, defaulting to 0 if not found
-}
getCountForEvent : String -> RsvpCounts -> Int
getCountForEvent eventId counts =
    Dict.get eventId counts
        |> Maybe.map .peopleCount
        |> Maybe.withDefault 0


{-| Format a count message for display. Returns Nothing if count < 2.
-}
formatCountMessage : Int -> Maybe String
formatCountMessage count =
    if count >= 2 then
        Just (String.fromInt count ++ " people have RSVP'd")

    else
        Nothing
