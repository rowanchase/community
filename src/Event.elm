module Event exposing (..)

import DateFormat
import Iso8601
import Time


type alias Url =
    String


type alias PhoneNumber =
    String


type alias EmailAddress =
    String


type alias DateTime =
    String


type alias Location =
    String


type alias Rsvp =
    { fullName : String
    , phone : Maybe PhoneNumber
    , email : Maybe EmailAddress
    , adults : Int
    , children : Int
    }


type RsvpConfig
    = NoRsvp
    | WithRsvp (List Rsvp)
    | ExternalRsvp Url


type alias Event =
    { title : String
    , description : String
    , start : DateTime
    , end : DateTime
    , location : Location
    , imageUrl : Maybe Url
    , rsvp : RsvpConfig
    }


sampleEvents : List Event
sampleEvents =
    [ { title = "Christmas Frolic"
      , description = "Annua Fryerstown Christmas celebration with Santa, food and music"
      , start = "2025-12-19T11:00:00"
      , end = "2025-12-19T15:00:00"
      , location = "Fryerstown Old School"
      , rsvp = WithRsvp []
      , imageUrl = Nothing
      }
    , { title = "CFA Open Day"
      , description = "Chance for the community to come see the CFA building, fire trucks and meet your local CFA members"
      , start = "2025-01-15T13:00:00"
      , end = "2025-01-15T14:00:00"
      , location = "Fryerstown CFA Building"
      , rsvp = NoRsvp
      , imageUrl = Nothing
      }
    , { title = "Car boot Sale"
      , description = "Annual Fryerstown Car boot sale"
      , start = "2026-03-03T10:00:00"
      , end = "2026-03-03T15:30:00"
      , location = "Fryerstown Old School"
      , rsvp = ExternalRsvp "someticketinglink"
      , imageUrl = Nothing
      }
    , { title = "Jim Cole's Birthday"
      , description = "Jim Cole is turning 90!"
      , start = "2026-01-01T15:00:00"
      , end = "2026-01-01T23:59:59"
      , location = "Fryerstown Old School"
      , rsvp = WithRsvp []
      , imageUrl = Nothing
      }
    , { title = "Rowan's Birthday"
      , description = "Rowan isn't turning 90 yet"
      , start = "2026-04-09T17:30:00"
      , end = "2026-04-09T23:59:59"
      , location = "Fryerstown Old School"
      , rsvp = NoRsvp
      , imageUrl = Nothing
      }
    ]


compareByStartTime : Event -> Event -> Order
compareByStartTime event1 event2 =
    case ( Iso8601.toTime event1.start, Iso8601.toTime event2.start ) of
        ( Ok posix1, Ok posix2 ) ->
            compare (Time.posixToMillis posix1) (Time.posixToMillis posix2)

        _ ->
            EQ


sortByStartTime : List Event -> List Event
sortByStartTime events =
    List.sortWith compareByStartTime events


isUpcoming : Time.Posix -> Event -> Bool
isUpcoming now event =
    case Iso8601.toTime event.start of
        Ok eventStart ->
            Time.posixToMillis eventStart >= Time.posixToMillis now

        Err _ ->
            False


posixToDateString : Time.Zone -> Time.Posix -> String
posixToDateString zone posix =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.text "-"
        , DateFormat.monthFixed
        , DateFormat.text "-"
        , DateFormat.dayOfMonthFixed
        ]
        zone
        posix


formatDateShort : Time.Zone -> String -> String
formatDateShort zone datetime =
    case Iso8601.toTime datetime of
        Ok posix ->
            String.toUpper
                (DateFormat.format
                    [ DateFormat.dayOfMonthFixed
                    , DateFormat.text " "
                    , DateFormat.monthNameAbbreviated
                    ]
                    zone
                    posix
                )

        Err _ ->
            datetime


formatTimeShort : Time.Zone -> Time.Posix -> String
formatTimeShort zone posix =
    let
        hour =
            DateFormat.format [ DateFormat.hourNumber ] zone posix

        minute =
            Time.toMinute zone posix

        ampm =
            DateFormat.format [ DateFormat.amPmLowercase ] zone posix

        maybeMinutes =
            if minute == 0 then
                ""

            else
                ":" ++ String.padLeft 2 '0' (String.fromInt minute)
    in
    hour ++ maybeMinutes ++ ampm


formatStartEndShort : Time.Zone -> String -> String -> String
formatStartEndShort zone start end =
    case ( Iso8601.toTime start, Iso8601.toTime end ) of
        ( Ok startPosix, Ok endPosix ) ->
            let
                isLate =
                    Time.toHour zone endPosix == 23 && Time.toMinute zone endPosix == 59

                isSameTime =
                    Time.posixToMillis startPosix == Time.posixToMillis endPosix

                isMultiDay =
                    Time.toDay zone startPosix /= Time.toDay zone endPosix
            in
            if isLate then
                formatTimeShort zone startPosix ++ " until late"

            else if isSameTime || isMultiDay then
                formatTimeShort zone startPosix

            else
                formatTimeShort zone startPosix ++ " until " ++ formatTimeShort zone endPosix

        _ ->
            start ++ " until " ++ end


getEventEndDate : Event -> String
getEventEndDate event =
    String.left 10 event.end


upcomingEvents : Time.Zone -> Time.Posix -> List Event -> List Event
upcomingEvents zone now events =
    let
        todayDate =
            posixToDateString zone now
    in
    List.filter (\event -> getEventEndDate event >= todayDate) events
