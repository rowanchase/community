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
