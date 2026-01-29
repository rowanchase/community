module Main exposing (Model, Msg(..), main, update)

import Browser
import Dict
import Event exposing (Event)
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import Json.Decode
import Rsvp exposing (RsvpFormData)
import RsvpCount exposing (RsvpCounts)
import Task
import Time



-- MODEL


type alias Flags =
    { supabaseUrl : String
    , supabaseAnonKey : String
    }


type RemoteData error data
    = NotAsked
    | Loading
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


type ModalState
    = EventModalOpen Event
    | RsvpModalOpen Event RsvpFormData (Maybe Event) (RemoteData String ())


type alias Model =
    { events : RemoteData String (List Event)
    , rsvpCounts : RemoteData String RsvpCounts
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    , townOrName : TownOrName
    , supabaseUrl : String
    , supabaseAnonKey : String
    , modalOpen : Maybe ModalState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { events = Loading
      , rsvpCounts = Loading
      , time = Nothing
      , zone = Nothing
      , townOrName = "Fryerstown"
      , supabaseUrl = flags.supabaseUrl
      , supabaseAnonKey = flags.supabaseAnonKey
      , modalOpen = Nothing
      }
    , Cmd.batch
        [ Task.perform ReceivedTime Time.now
        , Task.perform ReceivedZone Time.here
        , fetchEvents flags.supabaseUrl flags.supabaseAnonKey
        , fetchRsvpCounts flags.supabaseUrl flags.supabaseAnonKey
        ]
    )



-- UPDATE


type RsvpField
    = FullName
    | Adults
    | Children


type Msg
    = ReceivedTime Time.Posix
    | ReceivedZone Time.Zone
    | GotEvents (Result Http.Error (List Event))
    | GotRsvpCounts (Result Http.Error RsvpCounts)
    | OpenEventModal Event
    | OpenRsvpModal Event
    | CloseModal
    | UpdateRsvpField RsvpField String
    | SubmitRsvp
    | GotRsvpSubmissionResult (Result Http.Error ())


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


fetchRsvpCounts : String -> String -> Cmd Msg
fetchRsvpCounts supabaseUrl supabaseAnonKey =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "apikey" supabaseAnonKey
            , Http.header "Authorization" ("Bearer " ++ supabaseAnonKey)
            ]
        , url = supabaseUrl ++ "/rest/v1/event_rsvp_counts?select=*"
        , body = Http.emptyBody
        , expect = Http.expectJson GotRsvpCounts RsvpCount.rsvpCountsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


submitRsvp : String -> String -> String -> Rsvp.RsvpFormData -> Cmd Msg
submitRsvp supabaseUrl supabaseAnonKey eventId formData =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" supabaseAnonKey
            , Http.header "Authorization" ("Bearer " ++ supabaseAnonKey)
            , Http.header "Content-Type" "application/json"
            , Http.header "Prefer" "return=minimal"
            ]
        , url = supabaseUrl ++ "/rest/v1/rsvps"
        , body = Http.jsonBody (Rsvp.encodeRsvp eventId formData)
        , expect = Http.expectWhatever GotRsvpSubmissionResult
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

        GotRsvpCounts result ->
            case result of
                Ok counts ->
                    ( { model | rsvpCounts = Success counts }, Cmd.none )

                Err error ->
                    -- Silently fail - counts are non-critical
                    ( { model | rsvpCounts = Failure (httpErrorToString error) }, Cmd.none )

        OpenEventModal event ->
            ( { model | modalOpen = Just (EventModalOpen event) }, Cmd.none )

        OpenRsvpModal event ->
            let
                emptyRsvpForm =
                    { fullName = "", adults = "", children = "" }
            in
            case model.modalOpen of
                Just (EventModalOpen openEvent) ->
                    ( { model | modalOpen = Just (RsvpModalOpen event emptyRsvpForm (Just openEvent) NotAsked) }, Cmd.none )

                _ ->
                    ( { model | modalOpen = Just (RsvpModalOpen event emptyRsvpForm Nothing NotAsked) }, Cmd.none )

        CloseModal ->
            case model.modalOpen of
                Just modal ->
                    case modal of
                        RsvpModalOpen _ _ (Just previousEvent) _ ->
                            ( { model | modalOpen = Just (EventModalOpen previousEvent) }, Cmd.none )

                        _ ->
                            ( { model | modalOpen = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateRsvpField field value ->
            case model.modalOpen of
                Just (RsvpModalOpen event formData previousEvent submissionState) ->
                    let
                        updatedFormData =
                            case field of
                                FullName ->
                                    { formData | fullName = value }

                                Adults ->
                                    { formData | adults = value }

                                Children ->
                                    { formData | children = value }
                    in
                    ( { model | modalOpen = Just (RsvpModalOpen event updatedFormData previousEvent submissionState) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitRsvp ->
            case model.modalOpen of
                Just (RsvpModalOpen event formData previousEvent _) ->
                    if Rsvp.isRsvpFormValid formData then
                        ( { model | modalOpen = Just (RsvpModalOpen event formData previousEvent Loading) }
                        , submitRsvp model.supabaseUrl model.supabaseAnonKey event.id formData
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotRsvpSubmissionResult result ->
            case result of
                Ok () ->
                    case model.modalOpen of
                        Just (RsvpModalOpen _ _ (Just previousEvent) _) ->
                            ( { model | modalOpen = Just (EventModalOpen previousEvent) }, Cmd.none )

                        _ ->
                            ( { model | modalOpen = Nothing }, Cmd.none )

                Err error ->
                    case model.modalOpen of
                        Just (RsvpModalOpen event formData previousEvent _) ->
                            ( { model | modalOpen = Just (RsvpModalOpen event formData previousEvent (Failure (httpErrorToString error))) }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case ( model.time, model.zone ) of
        ( Just t, Just _ ) ->
            let
                counts =
                    case model.rsvpCounts of
                        Success c ->
                            c

                        _ ->
                            Dict.empty
            in
            div [ Html.Attributes.class "app" ]
                [ viewNavbar model.townOrName
                , viewEventsRemoteData t model.events
                , viewModal t counts model.modalOpen
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


viewEventsRemoteData : Time.Posix -> RemoteData String (List Event) -> Html Msg
viewEventsRemoteData now remoteData =
    case remoteData of
        NotAsked ->
            text ""

        Loading ->
            div [ Html.Attributes.class "loading" ] [ text "Loading events..." ]

        Success events ->
            viewEventList now events

        Failure error ->
            div [ Html.Attributes.class "error" ]
                [ text "Failed to load events: "
                , text error
                ]


viewEventList : Time.Posix -> List Event -> Html Msg
viewEventList now events =
    let
        upcomingEvents =
            Event.upcomingEvents now events
                |> Event.sortByStartTime
    in
    div [ Html.Attributes.class "event-list" ]
        (List.map viewEventCard upcomingEvents)


viewEventCard : Event -> Html Msg
viewEventCard event =
    div
        [ Html.Attributes.class "event-card"
        , onClick (OpenEventModal event)
        ]
        [ div [ Html.Attributes.class "event-date-box" ]
            [ text (Event.formatDateShort event.startTime) ]
        , div [ Html.Attributes.class "event-content" ]
            [ h1 [ Html.Attributes.class "event-title" ] [ text event.title ]
            , div [ Html.Attributes.class "event-time" ]
                [ text (Event.formatStartEndShort event.startTime event.endTime) ]
            , div [ Html.Attributes.class "event-rsvp" ]
                [ viewRsvpButton event ]
            ]
        ]


viewRsvpButton : Event -> Html Msg
viewRsvpButton event =
    case event.rsvp of
        Rsvp.NoAttendance ->
            Html.button
                [ Html.Attributes.class "event-rsvp-button"
                , onClick (OpenEventModal event)
                ]
                [ text "See Details" ]

        Rsvp.NoRsvp ->
            Html.button
                [ Html.Attributes.class "event-rsvp-button"
                , onClick (OpenEventModal event)
                ]
                [ text "All Welcome!" ]

        Rsvp.WithRsvp _ ->
            Html.button
                [ Html.Attributes.class "event-rsvp-button"
                , stopPropagationOn "click" (Json.Decode.succeed ( OpenRsvpModal event, True ))
                ]
                [ text "RSVP" ]

        Rsvp.ExternalRsvp url ->
            Html.a
                [ Html.Attributes.href url
                , Html.Attributes.target "_blank" -- opens in new tab
                , Html.Attributes.class "event-rsvp-button"
                ]
                [ text "Get Tickets" ]


viewModal : Time.Posix -> RsvpCounts -> Maybe ModalState -> Html Msg
viewModal now rsvpCounts maybeModal =
    case maybeModal of
        -- No modal open, render nothing
        Nothing ->
            text ""

        Just (EventModalOpen event) ->
            viewEventModal rsvpCounts event

        Just (RsvpModalOpen event formData _ submissionState) ->
            viewRsvpModal event formData submissionState


viewEventModal : RsvpCounts -> Event -> Html Msg
viewEventModal rsvpCounts event =
    let
        count =
            RsvpCount.getCountForEvent event.id rsvpCounts

        maybeCountMessage =
            RsvpCount.formatCountMessage count
    in
    div [ Html.Attributes.class "modal-backdrop", onClick CloseModal ]
        [ div
            [ Html.Attributes.class "modal-container"
            , stopPropagationOn "click" (Json.Decode.succeed ( CloseModal, True ))
            ]
            [ -- Close button in top right
              Html.button
                [ Html.Attributes.class "modal-close-button"
                , onClick CloseModal
                ]
                [ text "×" ]

            -- Event details
            , div [ Html.Attributes.class "modal-header" ]
                [ h1 [ Html.Attributes.class "modal-title" ] [ text event.title ]
                , div [ Html.Attributes.class "modal-date" ]
                    [ text (Event.formatDateShort event.startTime) ]
                , case maybeCountMessage of
                    Just message ->
                        div [ Html.Attributes.class "modal-rsvp-count" ]
                            [ text message ]

                    Nothing ->
                        text ""
                ]
            , div [ Html.Attributes.class "modal-body" ]
                [ div [ Html.Attributes.class "modal-time" ]
                    [ Html.strong [] [ text "When: " ]
                    , text (Event.formatStartEndShort event.startTime event.endTime)
                    ]
                , div [ Html.Attributes.class "modal-location" ]
                    [ Html.strong [] [ text "Where: " ]
                    , text event.location
                    ]
                , div [ Html.Attributes.class "modal-description" ]
                    [ text event.description ]
                ]
            ]
        ]


viewRsvpModal : Event -> RsvpFormData -> RemoteData String () -> Html Msg
viewRsvpModal event formData submissionState =
    div [ Html.Attributes.class "modal-backdrop" ]
        [ div
            [ Html.Attributes.class "modal-container"
            ]
            [ -- Close button in top right
              Html.button
                [ Html.Attributes.class "modal-close-button"
                , onClick CloseModal
                ]
                [ text "×" ]

            -- Modal header
            , div [ Html.Attributes.class "modal-header" ]
                [ h1 [ Html.Attributes.class "modal-title" ] [ text ("RSVP for " ++ event.title) ]
                ]

            -- Modal body with form
            , div [ Html.Attributes.class "modal-body" ]
                [ div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ]
                        [ text "Full Name" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.value formData.fullName
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.placeholder "Your full name"
                        , onInput (UpdateRsvpField FullName)
                        ]
                        []
                    ]
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ]
                        [ text "Adults" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.value formData.adults
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.placeholder "1"
                        , onInput (UpdateRsvpField Adults)
                        ]
                        []
                    ]
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ]
                        [ text "Children" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.value formData.children
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.placeholder "0"
                        , onInput (UpdateRsvpField Children)
                        ]
                        []
                    ]
                , Html.button
                    [ Html.Attributes.class "form-submit-button"
                    , Html.Attributes.disabled (not (Rsvp.isRsvpFormValid formData) || submissionState == Loading)
                    , onClick SubmitRsvp
                    ]
                    [ text
                        (if submissionState == Loading then
                            "Submitting..."

                         else
                            "Submit RSVP"
                        )
                    ]
                , case submissionState of
                    Failure errorMsg ->
                        div [ Html.Attributes.class "form-error" ]
                            [ text errorMsg ]

                    _ ->
                        text ""
                ]
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
