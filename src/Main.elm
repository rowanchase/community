module Main exposing (Model, Msg(..), main, update)

import Browser
import DateFormat
import Event exposing (Event, sampleEvents)
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Iso8601
import Task
import Time



-- MODEL


type alias TownOrName =
    String


type alias Model =
    { events : List Event
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    , townOrName : TownOrName
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { events = sampleEvents
      , time = Nothing
      , zone = Nothing
      , townOrName = "Fryerstown"
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
    | OpenEventModal Event
    | OpenRsvpModal Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedTime posix ->
            ( { model | time = Just posix }, Cmd.none )

        ReceivedZone zone ->
            ( { model | zone = Just zone }, Cmd.none )

        OpenEventModal event ->
            ( model, Cmd.none )

        OpenRsvpModal event ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case ( model.time, model.zone ) of
        ( Just t, Just z ) ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar model.townOrName
                , viewEventList t z model.events
                ]

        _ ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar model.townOrName
                , div [ Html.Attributes.class "loading" ] [ text "loading..." ]
                ]


viewNavbar : TownOrName -> Html Msg
viewNavbar townOrName =
    div [ Html.Attributes.class "navbar" ]
        [ h1 [ Html.Attributes.class "navbar-title" ]
            [ div [ Html.Attributes.class "navbar-page" ] [ text "What's on," ]
            , div [ Html.Attributes.class "navbar-town" ] [ text (townOrName ++ "?") ]
            ]
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
        [ div [ Html.Attributes.class "event-date-box" ]
            [ text (Event.formatDateShort zone event.start) ]
        , div [ Html.Attributes.class "event-content" ]
            [ h1 [ Html.Attributes.class "event-title" ] [ text event.title ]
            , div [ Html.Attributes.class "event-time" ]
                [ text (Event.formatStartEndShort zone event.start event.end) ]
            , div [ Html.Attributes.class "event-rsvp" ]
                [ getRsvpButton event ]
            ]
        ]


getRsvpButton : Event -> Html Msg
getRsvpButton event =
    case event.rsvp of
        Event.NoRsvp ->
            Html.button
                [ Html.Attributes.class "event-rsvp-button"
                , onClick (OpenEventModal event)
                ]
                [ text "All Welcome!" ]

        Event.WithRsvp _ ->
            Html.button
                [ Html.Attributes.class "event-rsvp-button"
                , onClick (OpenRsvpModal event)
                ]
                [ text "RSVP" ]

        Event.ExternalRsvp url ->
            Html.a
                [ Html.Attributes.href url
                , Html.Attributes.target "_blank" -- opens in new tab
                , Html.Attributes.class "event-rsvp-button"
                ]
                [ text "Get Tickets" ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
