port module Main exposing (Model, Msg(..), main, update)

import Auth exposing (AuthState(..), LoginFormState, User)
import Browser
import CreateEvent exposing (CreateEventFormData, RsvpConfigSelection(..))
import Dict
import Event exposing (Event)
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import Json.Decode
import RemoteData exposing (RemoteData(..))
import Rsvp exposing (RsvpFormData)
import RsvpCount exposing (RsvpCounts)
import Task
import Time



-- PORTS
-- Ports allow Elm to communicate with JavaScript
-- Outbound ports: Elm sends commands to JavaScript
-- Inbound ports: JavaScript sends data to Elm


{-| Request a magic link to be sent to the given email address
-}
port requestMagicLink : String -> Cmd msg


{-| Request to sign out the current user
-}
port requestSignOut : () -> Cmd msg


{-| Receive auth state changes from JavaScript
This fires when:

  - User signs in via magic link
  - User signs out
  - Page loads with existing session

-}
port authStateChanged : (Json.Decode.Value -> msg) -> Sub msg


{-| Receive result of magic link request
JavaScript sends True if successful, False if error
-}
port magicLinkSent : (Bool -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { supabaseUrl : String
    , supabaseAnonKey : String
    }


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
    | LoginModalOpen LoginFormState
    | CreateEventModalOpen CreateEventFormData (RemoteData String ())


type DraftForm
    = DraftCreateEvent CreateEventFormData
    | DraftRsvp RsvpFormData


type alias Model =
    { events : RemoteData String (List Event)
    , rsvpCounts : RemoteData String RsvpCounts
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    , townOrName : TownOrName
    , supabaseUrl : String
    , supabaseAnonKey : String
    , modalOpen : Maybe ModalState
    , authState : AuthState
    , draftForm : Maybe DraftForm
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
      , authState = SignedOut
      , draftForm = Nothing
      }
    , Cmd.batch
        [ Task.perform ReceivedTime Time.now
        , Task.perform ReceivedZone Time.here
        , fetchEvents flags.supabaseUrl flags.supabaseAnonKey
        , fetchRsvpCounts flags.supabaseUrl flags.supabaseAnonKey
        ]
    )



-- UPDATE


{-| Initialize an empty create event form
-}
initCreateEventFormData : CreateEventFormData
initCreateEventFormData =
    { title = ""
    , description = ""
    , startDate = ""
    , startTime = ""
    , endDate = ""
    , endTime = ""
    , location = ""
    , rsvpConfig = NoRsvpSelection
    , externalRsvpUrl = ""
    }


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
    | OpenLoginModal
    | UpdateLoginEmail String
    | SubmitMagicLinkRequest
    | MagicLinkSent Bool
    | AuthStateChanged Json.Decode.Value
    | SignOut
    | OpenCreateEventModal
    | UpdateEventTitle String
    | UpdateEventDescription String
    | UpdateEventStartDate String
    | UpdateEventStartTime String
    | UpdateEventEndDate String
    | UpdateEventEndTime String
    | UpdateEventLocation String
    | UpdateEventRsvpConfig RsvpConfigSelection
    | UpdateEventExternalRsvpUrl String
    | SubmitCreateEvent
    | GotCreateEventResult (Result Http.Error ())


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


createEvent : String -> String -> String -> CreateEvent.CreateEventFormData -> Cmd Msg
createEvent supabaseUrl supabaseAnonKey accessToken formData =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" supabaseAnonKey
            , Http.header "Authorization" ("Bearer " ++ accessToken) -- Use user's JWT token!
            , Http.header "Content-Type" "application/json"
            , Http.header "Prefer" "return=minimal"
            ]
        , url = supabaseUrl ++ "/rest/v1/events"
        , body = Http.jsonBody (CreateEvent.encodeEvent formData)
        , expect = Http.expectWhatever GotCreateEventResult
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
                -- Check if we have a saved draft for RSVP
                rsvpFormData =
                    case model.draftForm of
                        Just (DraftRsvp savedDraft) ->
                            savedDraft

                        -- Use saved draft
                        _ ->
                            { fullName = "", adults = "", children = "" }

                -- No draft, use empty form
                -- Track previous EventModal if one was open
                maybePreviousEvent =
                    case model.modalOpen of
                        Just (EventModalOpen openEvent) ->
                            Just openEvent

                        _ ->
                            Nothing
            in
            ( { model
                | modalOpen = Just (RsvpModalOpen event rsvpFormData maybePreviousEvent NotAsked)
                , draftForm = Just (DraftRsvp rsvpFormData) -- Save as draft immediately
              }
            , Cmd.none
            )

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
                    ( { model
                        | modalOpen = Just (RsvpModalOpen event updatedFormData previousEvent submissionState)
                        , draftForm = Just (DraftRsvp updatedFormData)
                      }
                    , Cmd.none
                    )

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
                            -- Return to EventModal, clear draft, fetch new counts
                            ( { model
                                | modalOpen = Just (EventModalOpen previousEvent)
                                , draftForm = Nothing -- Clear draft on success!
                              }
                            , fetchRsvpCounts model.supabaseUrl model.supabaseAnonKey
                            )

                        _ ->
                            -- Close modal, clear draft, fetch new counts
                            ( { model
                                | modalOpen = Nothing
                                , draftForm = Nothing -- Clear draft on success!
                              }
                            , fetchRsvpCounts model.supabaseUrl model.supabaseAnonKey
                            )

                Err error ->
                    -- Error: keep modal open, keep draft, show error
                    case model.modalOpen of
                        Just (RsvpModalOpen event formData previousEvent _) ->
                            ( { model | modalOpen = Just (RsvpModalOpen event formData previousEvent (Failure (httpErrorToString error))) }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        OpenLoginModal ->
            ( { model
                | modalOpen = Just (LoginModalOpen { email = "", status = NotAsked })
              }
            , Cmd.none
            )

        UpdateLoginEmail email ->
            case model.modalOpen of
                Just (LoginModalOpen formState) ->
                    ( { model
                        | modalOpen = Just (LoginModalOpen { formState | email = email })
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitMagicLinkRequest ->
            case model.modalOpen of
                Just (LoginModalOpen formState) ->
                    if Auth.isValidEmail formState.email then
                        ( { model
                            | modalOpen = Just (LoginModalOpen { formState | status = Loading })
                          }
                        , requestMagicLink formState.email
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MagicLinkSent success ->
            case model.modalOpen of
                Just (LoginModalOpen formState) ->
                    if success then
                        ( { model
                            | modalOpen = Just (LoginModalOpen { formState | status = Success () })
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | modalOpen = Just (LoginModalOpen { formState | status = Failure "Failed to send magic link. Please try again." })
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        AuthStateChanged jsonValue ->
            case Json.Decode.decodeValue Auth.authStateDecoder jsonValue of
                Ok authState ->
                    ( { model
                        | authState = authState
                        , modalOpen = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    -- Failed to decode auth state - ignore it
                    ( model, Cmd.none )

        SignOut ->
            ( model, requestSignOut () )

        OpenCreateEventModal ->
            let
                -- Check if we have a saved draft for Create Event
                formData =
                    case model.draftForm of
                        Just (DraftCreateEvent savedDraft) ->
                            savedDraft

                        -- Use saved draft
                        _ ->
                            initCreateEventFormData

                -- No draft, use empty form
            in
            ( { model
                | modalOpen = Just (CreateEventModalOpen formData NotAsked)
                , draftForm = Just (DraftCreateEvent formData) -- Save as draft immediately
              }
            , Cmd.none
            )

        UpdateEventTitle newTitle ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | title = newTitle }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventDescription newDescription ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | description = newDescription }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventStartDate newStartDate ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | startDate = newStartDate }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventStartTime newStartTime ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | startTime = newStartTime }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventEndDate newEndDate ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | endDate = newEndDate }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventEndTime newEndTime ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | endTime = newEndTime }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventLocation newLocation ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | location = newLocation }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventRsvpConfig newRsvpConfig ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | rsvpConfig = newRsvpConfig }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventExternalRsvpUrl newUrl ->
            case model.modalOpen of
                Just (CreateEventModalOpen formData status) ->
                    let
                        updatedFormData =
                            { formData | externalRsvpUrl = newUrl }
                    in
                    ( { model
                        | modalOpen = Just (CreateEventModalOpen updatedFormData status)
                        , draftForm = Just (DraftCreateEvent updatedFormData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitCreateEvent ->
            case ( model.modalOpen, model.authState ) of
                ( Just (CreateEventModalOpen formData _), SignedIn user ) ->
                    if CreateEvent.isFormValid formData then
                        ( { model | modalOpen = Just (CreateEventModalOpen formData Loading) }
                        , createEvent model.supabaseUrl model.supabaseAnonKey user.accessToken formData
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotCreateEventResult result ->
            case result of
                Ok () ->
                    -- Success: close modal, clear draft, refresh events list
                    ( { model
                        | modalOpen = Nothing
                        , draftForm = Nothing -- Clear draft on success!
                      }
                    , fetchEvents model.supabaseUrl model.supabaseAnonKey
                    )

                Err httpError ->
                    -- Error: keep modal open, keep draft, show error
                    case model.modalOpen of
                        Just (CreateEventModalOpen formData _) ->
                            ( { model | modalOpen = Just (CreateEventModalOpen formData (Failure (httpErrorToString httpError))) }
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
                , viewEventsRemoteData model.authState t model.events
                , viewModal t counts model.modalOpen
                , Html.footer
                    [ Html.Attributes.class "app-footer" ]
                    [ viewLoginButton model.authState ]
                ]

        _ ->
            div [ Html.Attributes.class "app" ]
                [ viewNavbar model.townOrName
                , div [ Html.Attributes.class "loading" ] [ text "loading..." ]
                , Html.footer
                    [ Html.Attributes.class "app-footer" ]
                    [ viewLoginButton model.authState ]
                ]


viewNavbar : TownOrName -> Html Msg
viewNavbar townOrName =
    div [ Html.Attributes.class "navbar" ]
        [ h1 [ Html.Attributes.class "navbar-title" ]
            [ div [ Html.Attributes.class "navbar-page" ] [ text "What's on," ]
            , div [ Html.Attributes.class "navbar-town" ] [ text (townOrName ++ "?") ]
            ]
        ]


viewEventsRemoteData : AuthState -> Time.Posix -> RemoteData String (List Event) -> Html Msg
viewEventsRemoteData authState now remoteData =
    case remoteData of
        NotAsked ->
            text ""

        Loading ->
            div [ Html.Attributes.class "loading" ] [ text "Loading events..." ]

        Success events ->
            viewEventList authState now events

        Failure error ->
            div [ Html.Attributes.class "error" ]
                [ text "Failed to load events: "
                , text error
                ]


viewEventList : AuthState -> Time.Posix -> List Event -> Html Msg
viewEventList authState now events =
    let
        upcomingEvents =
            Event.upcomingEvents now events
                |> Event.sortByStartTime
    in
    div [ Html.Attributes.class "event-list" ]
        (viewNewEventButton authState
            :: List.map viewEventCard upcomingEvents
        )


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


viewNewEventButton : AuthState -> Html Msg
viewNewEventButton authState =
    case authState of
        SignedIn _ ->
            div [ Html.Attributes.class "new-event-button-container" ]
                [ Html.button
                    [ Html.Attributes.class "new-event-button"
                    , onClick OpenCreateEventModal
                    ]
                    [ text "+ New Event" ]
                ]

        SignedOut ->
            text ""


viewLoginButton : AuthState -> Html Msg
viewLoginButton authState =
    div [ Html.Attributes.class "login-button-container" ]
        [ case authState of
            SignedOut ->
                Html.button
                    [ Html.Attributes.class "login-button"
                    , onClick OpenLoginModal
                    ]
                    [ text "Sign in" ]

            SignedIn user ->
                div [ Html.Attributes.class "user-info" ]
                    [ text user.email
                    , Html.button
                        [ Html.Attributes.class "signout-button"
                        , onClick SignOut
                        ]
                        [ text "Sign out" ]
                    ]
        ]


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

        Just (LoginModalOpen formState) ->
            viewLoginModal formState

        Just (CreateEventModalOpen formData status) ->
            viewCreateEventModal formData status


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
                , viewModalRsvpButton event
                ]
            ]
        ]


viewModalRsvpButton : Event -> Html Msg
viewModalRsvpButton event =
    case event.rsvp of
        Rsvp.WithRsvp _ ->
            div [ Html.Attributes.class "modal-rsvp-button-container" ]
                [ Html.button
                    [ Html.Attributes.class "event-rsvp-button"
                    , onClick (OpenRsvpModal event)
                    ]
                    [ text "RSVP" ]
                ]

        Rsvp.ExternalRsvp url ->
            div [ Html.Attributes.class "modal-rsvp-button-container" ]
                [ Html.a
                    [ Html.Attributes.href url
                    , Html.Attributes.target "_blank"
                    , Html.Attributes.class "event-rsvp-button"
                    ]
                    [ text "Get Tickets" ]
                ]

        _ ->
            text ""


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


rsvpConfigFromString : String -> RsvpConfigSelection
rsvpConfigFromString str =
    case str of
        "no_attendance" ->
            NoAttendanceSelection

        "with_rsvp" ->
            WithRsvpSelection

        "external_rsvp" ->
            ExternalRsvpSelection

        _ ->
            -- default
            NoRsvpSelection


viewCreateEventModal : CreateEventFormData -> RemoteData String () -> Html Msg
viewCreateEventModal formData status =
    div [ Html.Attributes.class "modal-backdrop" ]
        [ div
            [ Html.Attributes.class "modal-container"
            ]
            [ -- Modal close button (×)
              Html.button
                [ Html.Attributes.class "modal-close-button"
                , onClick CloseModal
                ]
                [ text "×" ]

            -- Modal header
            , div [ Html.Attributes.class "modal-header" ]
                [ h1 [ Html.Attributes.class "modal-title" ] [ text "Create New Event" ]
                ]

            -- Modal body with form
            , div [ Html.Attributes.class "modal-body" ]
                [ -- Title field
                  div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "Event Title" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.placeholder "Community BBQ"
                        , Html.Attributes.value formData.title
                        , onInput UpdateEventTitle
                        ]
                        []
                    ]

                -- Description field (textarea for multiline)
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "Description" ]
                    , Html.textarea
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.rows 4
                        , Html.Attributes.placeholder "Tell people about the event..."
                        , Html.Attributes.value formData.description
                        , onInput UpdateEventDescription
                        ]
                        []
                    ]

                -- Start Date field
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "Start Date" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "date"
                        , Html.Attributes.value formData.startDate
                        , onInput UpdateEventStartDate
                        ]
                        []
                    ]

                -- Start Time field
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "Start Time" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "time"
                        , Html.Attributes.value formData.startTime
                        , onInput UpdateEventStartTime
                        ]
                        []
                    ]

                -- End Date field
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "End Date" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "date"
                        , Html.Attributes.value formData.endDate
                        , onInput UpdateEventEndDate
                        ]
                        []
                    , case CreateEvent.validateEndTimeMessage formData.startDate formData.startTime formData.endDate formData.endTime of
                        Just errorMsg ->
                            div [ Html.Attributes.class "form-field-error" ]
                                [ text errorMsg ]

                        Nothing ->
                            text ""
                    ]

                -- End Time field
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "End Time" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "time"
                        , Html.Attributes.value formData.endTime
                        , onInput UpdateEventEndTime
                        ]
                        []
                    , case CreateEvent.validateEndTimeMessage formData.startDate formData.startTime formData.endDate formData.endTime of
                        Just errorMsg ->
                            div [ Html.Attributes.class "form-field-error" ]
                                [ text errorMsg ]

                        Nothing ->
                            text ""
                    ]

                -- Location field
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "Location" ]
                    , Html.input
                        [ Html.Attributes.class "form-input"
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.placeholder "Fryerstown Old School"
                        , Html.Attributes.value formData.location
                        , onInput UpdateEventLocation
                        ]
                        []
                    ]

                -- RSVP Config dropdown
                , div [ Html.Attributes.class "form-field" ]
                    [ Html.label [ Html.Attributes.class "form-label" ] [ text "RSVP Type" ]
                    , Html.select
                        [ Html.Attributes.class "form-input"
                        , onInput (rsvpConfigFromString >> UpdateEventRsvpConfig)
                        ]
                        [ Html.option [ Html.Attributes.value "no_rsvp" ] [ text "Anyone Welcome, No RSVP Required" ]
                        , Html.option [ Html.Attributes.value "with_rsvp" ] [ text "Requires RSVP (free)" ]
                        , Html.option [ Html.Attributes.value "external_rsvp" ] [ text "Ticketed ($$)" ]
                        , Html.option [ Html.Attributes.value "no_attendance" ] [ text "No Attendence Required" ]
                        ]
                    ]

                -- External RSVP URL (conditional)
                , case formData.rsvpConfig of
                    ExternalRsvpSelection ->
                        div [ Html.Attributes.class "form-field" ]
                            [ Html.label [ Html.Attributes.class "form-label" ] [ text "Link to Buy Tickets" ]
                            , Html.input
                                [ Html.Attributes.class "form-input"
                                , Html.Attributes.type_ "url"
                                , Html.Attributes.placeholder "https://example.com/rsvp"
                                , Html.Attributes.value formData.externalRsvpUrl
                                , onInput UpdateEventExternalRsvpUrl
                                ]
                                []
                            , case CreateEvent.validateExternalUrlMessage formData.externalRsvpUrl of
                                Just errorMsg ->
                                    div [ Html.Attributes.class "form-field-error" ]
                                        [ text errorMsg ]

                                Nothing ->
                                    text ""
                            ]

                    _ ->
                        text ""

                -- Submit button
                , Html.button
                    [ Html.Attributes.class "form-submit-button"
                    , Html.Attributes.disabled (not (CreateEvent.isFormValid formData) || status == Loading)
                    , onClick SubmitCreateEvent
                    ]
                    [ text
                        (if status == Loading then
                            "Creating..."

                         else
                            "Create Event"
                        )
                    ]

                -- Error message
                , case status of
                    Failure errorMsg ->
                        div [ Html.Attributes.class "form-error" ]
                            [ text errorMsg ]

                    _ ->
                        text ""
                ]
            ]
        ]


viewLoginModal : LoginFormState -> Html Msg
viewLoginModal formState =
    div [ Html.Attributes.class "modal-backdrop" ]
        [ div
            [ Html.Attributes.class "modal-container"
            ]
            [ -- Modal close button
              Html.button
                [ Html.Attributes.class "modal-close-button"
                , onClick CloseModal
                ]
                [ text "×" ]
            , -- Modal header
              div [ Html.Attributes.class "modal-header" ]
                [ h1 [ Html.Attributes.class "modal-title" ] [ text "Sign In" ]
                ]
            , -- Modal body
              div [ Html.Attributes.class "modal-body" ]
                [ case formState.status of
                    Success () ->
                        div [ Html.Attributes.class "login-success" ]
                            [ text "✓ Check your email! Click the link to sign in." ]

                    _ ->
                        div []
                            [ Html.p [] [ text "Enter your email to receive a magic link" ]
                            , -- Email input
                              div [ Html.Attributes.class "form-field" ]
                                [ Html.label [ Html.Attributes.class "form-label" ]
                                    [ text "Email" ]
                                , Html.input
                                    [ Html.Attributes.class "form-input"
                                    , Html.Attributes.value formState.email
                                    , Html.Attributes.type_ "email"
                                    , Html.Attributes.placeholder "you@example.com"
                                    , onInput UpdateLoginEmail
                                    ]
                                    []
                                ]
                            , -- Submit button
                              Html.button
                                [ Html.Attributes.class "form-submit-button"
                                , Html.Attributes.disabled
                                    (not (Auth.isValidEmail formState.email) || formState.status == Loading)
                                , onClick SubmitMagicLinkRequest
                                ]
                                [ text
                                    (if formState.status == Loading then
                                        "Sending..."

                                     else
                                        "Send Magic Link"
                                    )
                                ]
                            , -- Error message
                              case formState.status of
                                Failure errorMsg ->
                                    div [ Html.Attributes.class "form-error" ]
                                        [ text errorMsg ]

                                _ ->
                                    text ""
                            ]
                ]
            ]
        ]



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ authStateChanged AuthStateChanged
        , magicLinkSent MagicLinkSent
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
