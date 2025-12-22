module MainTest exposing (..)

import Expect
import Main exposing (Msg(..), update)
import Test exposing (..)
import Event exposing (RsvpConfig(..))


suite : Test
suite =
    describe "Event Navigation"
        [ test "NextEvent increases currentIndex" <|
            \_ ->
                let
                    initialModel =
                        { events =
                            [ { title = "E1", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            , { title = "E2", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            ]
                        , currentIndex = 0
                        }

                    newModel =
                        update NextEvent initialModel
                in
                Expect.equal newModel.currentIndex 1
        , test "PreviousEvent decreases currentIndex" <|
            \_ ->
                let
                    initialModel =
                        { events =
                            [ { title = "E1", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            , { title = "E2", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            ]
                        , currentIndex = 2
                        }

                    newModel =
                        update PreviousEvent initialModel
                in
                Expect.equal newModel.currentIndex 1
        , test "PreviousEvent at index 0 stays at 0" <|
            \_ ->
                let
                    initialModel =
                        { events = []
                        , currentIndex = 0
                        }

                    newModel =
                        update PreviousEvent initialModel
                in
                Expect.equal newModel.currentIndex 0
        , test "NextEvent at last index stays at last index" <|
            \_ ->
                let
                    initialModel =
                        { events =
                            [ { title = "E1", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            , { title = "E2", description = "", start = "", end = "", location = "", rsvp = NoRsvp, imageUrl = Nothing }
                            ]
                        , currentIndex = 1
                        }

                    newModel =
                        update NextEvent initialModel
                in
                Expect.equal newModel.currentIndex 1
        ]
