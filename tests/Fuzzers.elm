module Fuzzers exposing (..)

import Event exposing (DateTime, Event)
import Fuzz exposing (Fuzzer)
import Rsvp exposing (Rsvp, RsvpConfig(..))
import RsvpCount exposing (RsvpCount)


startAndEnd : Fuzzer ( DateTime, DateTime )
startAndEnd =
    Fuzz.map2 Tuple.pair dateTime dateTime
        |> Fuzz.map
            (\( dt1, dt2 ) ->
                -- Sort them so earlier is first
                if dt1 <= dt2 then
                    ( dt1, dt2 )

                else
                    ( dt2, dt1 )
            )


event : Fuzzer Event
event =
    Fuzz.map7
        (\id ( startTime, endTime ) title description location imageUrl rsvpCfg ->
            { id = id
            , title = title
            , description = description
            , startTime = startTime
            , endTime = endTime
            , location = location
            , imageUrl = imageUrl
            , rsvp = rsvpCfg
            }
        )
        Fuzz.string
        startAndEnd
        Fuzz.string
        Fuzz.string
        Fuzz.string
        (Fuzz.maybe Fuzz.string)
        rsvpConfig


rsvp : Fuzzer Rsvp
rsvp =
    Fuzz.map5 Rsvp
        Fuzz.string
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.intRange 0 10)
        (Fuzz.intRange 0 10)


rsvpConfig : Fuzzer RsvpConfig
rsvpConfig =
    Fuzz.oneOf
        [ Fuzz.constant NoRsvp
        , Fuzz.map WithRsvp (Fuzz.list rsvp)
        , Fuzz.map ExternalRsvp Fuzz.string
        ]


rsvpCount : Fuzzer RsvpCount
rsvpCount =
    Fuzz.map5
        (\eventId rsvpCnt adultCnt childrenCnt peopleCnt ->
            { eventId = eventId
            , rsvpCount = rsvpCnt
            , adultCount = adultCnt
            , childrenCount = childrenCnt
            , peopleCount = peopleCnt
            }
        )
        Fuzz.string
        (Fuzz.intRange 0 100)
        (Fuzz.intRange 0 100)
        (Fuzz.intRange 0 100)
        (Fuzz.intRange 0 200)


dateTime : Fuzzer String
dateTime =
    Fuzz.map6
        (\year month day hour minute second ->
            String.fromInt year
                ++ "-"
                ++ String.padLeft 2 '0' (String.fromInt month)
                ++ "-"
                ++ String.padLeft 2 '0' (String.fromInt day)
                ++ "T"
                ++ String.padLeft 2 '0' (String.fromInt hour)
                ++ ":"
                ++ String.padLeft 2 '0' (String.fromInt minute)
                ++ ":"
                ++ String.padLeft 2 '0' (String.fromInt second)
        )
        (Fuzz.intRange 2020 2030)
        -- year
        (Fuzz.intRange 1 12)
        -- month
        (Fuzz.intRange 1 28)
        -- day
        (Fuzz.intRange 0 23)
        -- hour
        (Fuzz.intRange 0 59)
        -- minute
        (Fuzz.intRange 0 59)



-- second
