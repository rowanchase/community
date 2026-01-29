module Main exposing (Model, Msg(..), main, update)

import Browser
import Event exposing (Event)
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Http
import Json.Decode
import Task
import Time



-- MODEL


type alias Flags =
    { supabaseUrl : String
    , supabaseAnonKey : String
    }


type RemoteData error data
    = Loading
    | Success data
    | Failure error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error - check your connection"

        Http.BadStatus status ->
            "Server returned error: " ++ String.fromInt status

        Http.BadBody message ->
            "Failed to decode response: " ++ message


type alias TownOrName =
    String


type alias Model =
    { events : RemoteData String (List Event)
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    , townOrName : TownOrName
    , supabaseUrl : String
    , supabaseAnonKey : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { events = Loading
      , time = Nothing
      , zone = Nothing
      , townOrName = "Fryerstown"
      , supabaseUrl = flags.supabaseUrl
      , supabaseAnonKey = flags.supabaseAnonKey
      }
    , Cmd.batch
        [ Task.perform ReceivedTime Time.now
        , Task.perform ReceivedZone Time.here
        , fetchEvents flags.supabaseUrl flags.supabaseAnonKey
        ]
    )



-- UPDATE


type Msg
    = ReceivedTime Time.Posix
    | ReceivedZone Time.Zone
    | GotEvents (Result Http.Error (List Event))


fetchEvents : String -> String -> Cmd Msg
fetchEvents supabaseUrl supabaseAnonKey =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "apikey" supabaseAnonKey
            , Http.header "Authorization" ("Bearer " ++ supabaseAnonKey)
            ]
        , url = supabaseUrl ++ "/rest/v1/events?select=*"
        , body = Http.emptyBody
        , expect = Http.expectJson GotEvents (Json.Decode.list Event.eventDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedTime posix ->
            ( { model | time = Just posix }, Cmd.none )

        ReceivedZone zone ->
            ( { model | zone = Just zone }, Cmd.none )

        GotEvents result ->
            case result of
                Ok events ->
                    ( { model | events = Success events }, Cmd.none )

                Err error ->
                    ( { model | events = Failure (httpErrorToString error) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case ( model.time, model.zone ) of
        ( Just t, Just z ) ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar model.townOrName
                , viewEventsRemoteData t z model.events
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


viewEventsRemoteData : Time.Posix -> Time.Zone -> RemoteData String (List Event) -> Html Msg
viewEventsRemoteData now zone remoteData =
    case remoteData of
        Loading ->
            div [ Html.Attributes.class "loading" ] [ text "Loading events..." ]

        Success events ->
            viewEventList now zone events

        Failure error ->
            div [ Html.Attributes.class "error" ]
                [ text "Failed to load events: "
                , text error
                ]


viewEventList : Time.Posix -> Time.Zone -> List Event -> Html Msg
viewEventList now zone events =
    let
        upcomingEvents =
            Event.upcomingEvents zone now events
                |> Event.sortByStartTime
    in
    div [ Html.Attributes.class "event-list" ]
        (List.map viewEventCard upcomingEvents)


viewEventCard : Event -> Html Msg
viewEventCard event =
    div [ Html.Attributes.class "event-card" ]
        [ div [ Html.Attributes.class "event-date-box" ]
            [ text (Event.formatDateShort event.startTime) ]
        , div [ Html.Attributes.class "event-content" ]
            [ h1 [ Html.Attributes.class "event-title" ] [ text event.title ]
            , div [ Html.Attributes.class "event-description" ] [ text event.description ]
            ]
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
