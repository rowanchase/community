module RsvpCountTest exposing (..)

import Dict
import Expect
import Fuzz
import Fuzzers
import Json.Decode as Decode
import RsvpCount exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "RsvpCount"
        [ describe "rsvpCountDecoder"
            [ test "decodes valid RSVP count JSON" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "event_id": "abc123",
                                "rsvp_count": 5,
                                "adult_count": 8,
                                "children_count": 2,
                                "people_count": 10
                            }
                            """

                        expected =
                            { eventId = "abc123"
                            , rsvpCount = 5
                            , adultCount = 8
                            , childrenCount = 2
                            , peopleCount = 10
                            }
                    in
                    Decode.decodeString rsvpCountDecoder json
                        |> Expect.equal (Ok expected)
            ]
        , describe "rsvpCountsDecoder"
            [ test "converts list to Dict keyed by event_id" <|
                \_ ->
                    let
                        json =
                            """
                            [
                                {
                                    "event_id": "event-1",
                                    "rsvp_count": 3,
                                    "adult_count": 4,
                                    "children_count": 1,
                                    "people_count": 5
                                },
                                {
                                    "event_id": "event-2",
                                    "rsvp_count": 2,
                                    "adult_count": 2,
                                    "children_count": 0,
                                    "people_count": 2
                                }
                            ]
                            """

                        result =
                            Decode.decodeString rsvpCountsDecoder json
                    in
                    case result of
                        Ok counts ->
                            Expect.all
                                [ \_ -> Dict.size counts |> Expect.equal 2
                                , \_ -> Dict.get "event-1" counts |> Maybe.map .peopleCount |> Expect.equal (Just 5)
                                , \_ -> Dict.get "event-2" counts |> Maybe.map .peopleCount |> Expect.equal (Just 2)
                                ]
                                ()

                        Err _ ->
                            Expect.fail "Failed to decode JSON"
            , test "handles empty list" <|
                \_ ->
                    Decode.decodeString rsvpCountsDecoder "[]"
                        |> Expect.equal (Ok Dict.empty)
            ]
        , describe "formatCountMessage"
            [ test "returns Nothing when count is 0" <|
                \_ ->
                    formatCountMessage 0
                        |> Expect.equal Nothing
            , test "returns Nothing when count is 1" <|
                \_ ->
                    formatCountMessage 1
                        |> Expect.equal Nothing
            , test "returns message when count is 2" <|
                \_ ->
                    formatCountMessage 2
                        |> Expect.equal (Just "2 people are coming so far!")
            , test "returns message when count is 15" <|
                \_ ->
                    formatCountMessage 15
                        |> Expect.equal (Just "15 people are coming so far!")
            , fuzz (Fuzz.intRange 2 1000) "always returns message for count >= 2" <|
                \count ->
                    formatCountMessage count
                        |> Expect.notEqual Nothing
            , fuzz (Fuzz.intRange -100 1) "never returns message for count < 2" <|
                \count ->
                    formatCountMessage count
                        |> Expect.equal Nothing
            ]
        , describe "getCountForEvent"
            [ test "returns 0 for missing event ID" <|
                \_ ->
                    let
                        counts =
                            Dict.empty
                    in
                    getCountForEvent "missing-event" counts
                        |> Expect.equal 0
            , test "returns peopleCount for existing event" <|
                \_ ->
                    let
                        counts =
                            Dict.fromList
                                [ ( "event-1"
                                  , { eventId = "event-1"
                                    , rsvpCount = 5
                                    , adultCount = 8
                                    , childrenCount = 2
                                    , peopleCount = 10
                                    }
                                  )
                                ]
                    in
                    getCountForEvent "event-1" counts
                        |> Expect.equal 10
            , fuzz Fuzzers.rsvpCount "returns peopleCount when event exists in dict" <|
                \count ->
                    let
                        counts =
                            Dict.fromList [ ( count.eventId, count ) ]
                    in
                    getCountForEvent count.eventId counts
                        |> Expect.equal count.peopleCount
            ]
        ]
