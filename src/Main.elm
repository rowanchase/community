module Main exposing (Model, Msg(..), main, update)

import Browser
import DateFormat
import Event exposing (Event, sampleEvents)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Iso8601
import Task
import Time



-- MODEL


type alias Model =
    { events : List Event
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { events = sampleEvents
      , time = Nothing
      , zone = Nothing
      }
    , Cmd.batch
        [ Task.perform ReceivedTime Time.now
        , Task.perform ReceivedZone Time.here
        ]
    )



-- UPDATE


type Msg
    = ReceivedTime Time.Posix
    | ReceivedZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedTime posix ->
            ( { model | time = Just posix }, Cmd.none )

        ReceivedZone zone ->
            ( { model | zone = Just zone }, Cmd.none )



-- VIEW


formatDateTime : Time.Zone -> String -> String
formatDateTime zone isoString =
    case Iso8601.toTime isoString of
        Ok posix ->
            DateFormat.format
                [ DateFormat.dayOfWeekNameFull
                , DateFormat.text " "
                , DateFormat.dayOfMonthNumber
                , DateFormat.text " "
                , DateFormat.monthNameFull
                , DateFormat.text " "
                , DateFormat.yearNumber
                , DateFormat.text ", "
                , DateFormat.hourNumber
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                , DateFormat.amPmLowercase
                ]
                zone
                posix

        Err _ ->
            isoString


view : Model -> Html Msg
view model =
    case ( model.time, model.zone ) of
        ( Just t, Just z ) ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar
                , viewEventList t z model.events
                ]

        _ ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar
                , div [ Html.Attributes.class "loading" ] [ text "loading..." ]
                ]


viewNavbar : Html Msg
viewNavbar =
    div [ Html.Attributes.class "navbar" ]
        [ h1 [ Html.Attributes.class "navbar-title" ]
            [ text "WHAT'S ON, FRYERSTOWN?" ]
        ]


viewEventList : Time.Posix -> Time.Zone -> List Event -> Html Msg
viewEventList now zone events =
    let
        upcomingEvents =
            Event.upcomingEvents zone now events
                |> Event.sortByStartTime
    in
    div [ Html.Attributes.class "event-list" ]
        (List.map (viewEventCard zone) upcomingEvents)


viewEventCard : Time.Zone -> Event -> Html Msg
viewEventCard zone event =
    div [ Html.Attributes.class "event-card" ]
        [ h1 [ Html.Attributes.class "event-title" ] [ text event.title ]
        , div [ Html.Attributes.class "event-description" ] [ text event.description ]
        , div [ Html.Attributes.class "event-time" ]
            [ text ("Starts: " ++ formatDateTime zone event.start) ]
        , div [ Html.Attributes.class "event-time" ]
            [ text ("Ends: " ++ formatDateTime zone event.end) ]
        , div [ Html.Attributes.class "event-location" ]
            [ text ("Location: " ++ event.location) ]
        , viewRsvpSection event.rsvp
        ]


viewRsvpSection : Event.RsvpConfig -> Html Msg
viewRsvpSection rsvpConfig =
    case rsvpConfig of
        Event.NoRsvp ->
            div [] []

        Event.WithRsvp rsvps ->
            div [ Html.Attributes.class "rsvp-section" ]
                [ h2 [ Html.Attributes.class "rsvp-title" ] [ text "RSVP" ]
                , div [ Html.Attributes.class "rsvp-count" ]
                    [ text ("Current RSVPs: " ++ String.fromInt (List.length rsvps)) ]
                ]

        Event.ExternalRsvp url ->
            div [ Html.Attributes.class "rsvp-section" ]
                [ h2 [ Html.Attributes.class "rsvp-title" ] [ text "RSVP" ]
                , Html.a
                    [ Html.Attributes.href url
                    , Html.Attributes.class "rsvp-link"
                    ]
                    [ text "Get tickets here" ]
                ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
