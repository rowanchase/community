module AuthTest exposing (..)

import Auth exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Test exposing (..)


{-| Test suite for email validation

We test both explicit examples (specific valid/invalid cases) and fuzz testing
(randomly generated strings) to catch edge cases.
-}
emailValidationTests : Test
emailValidationTests =
    describe "Email validation"
        [ describe "Valid emails"
            [ test "accepts simple email" <|
                \_ ->
                    isValidEmail "test@example.com"
                        |> Expect.equal True
            , test "accepts email with dots" <|
                \_ ->
                    isValidEmail "first.last@example.com"
                        |> Expect.equal True
            , test "accepts email with plus" <|
                \_ ->
                    isValidEmail "user+tag@example.com"
                        |> Expect.equal True
            , test "accepts email with subdomain" <|
                \_ ->
                    isValidEmail "user@mail.example.com"
                        |> Expect.equal True
            ]
        , describe "Invalid emails"
            [ test "rejects empty string" <|
                \_ ->
                    isValidEmail ""
                        |> Expect.equal False
            , test "rejects email without @" <|
                \_ ->
                    isValidEmail "notanemail.com"
                        |> Expect.equal False
            , test "rejects email without domain" <|
                \_ ->
                    isValidEmail "user@"
                        |> Expect.equal False
            , test "rejects email without local part" <|
                \_ ->
                    isValidEmail "@example.com"
                        |> Expect.equal False
            , test "rejects very short string" <|
                \_ ->
                    isValidEmail "a@b"
                        |> Expect.equal False
            ]
        , fuzz Fuzz.string "never crashes on any string input" <|
            \randomString ->
                -- This test ensures our validation function is robust
                -- It should never crash, regardless of input
                let
                    _ =
                        isValidEmail randomString
                in
                Expect.pass
        ]


{-| Test suite for User JSON decoder

We need to decode user data coming from JavaScript (via ports).
The JSON looks like: {"id": "123", "email": "user@example.com"}
-}
userDecoderTests : Test
userDecoderTests =
    describe "User decoder"
        [ test "decodes valid user JSON" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "id": "user-123",
                            "email": "test@example.com"
                        }
                        """
                in
                Decode.decodeString userDecoder json
                    |> Expect.equal
                        (Ok
                            { id = "user-123"
                            , email = "test@example.com"
                            }
                        )
        , test "fails on missing id field" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "email": "test@example.com"
                        }
                        """
                in
                Decode.decodeString userDecoder json
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails on missing email field" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "id": "user-123"
                        }
                        """
                in
                Decode.decodeString userDecoder json
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


{-| Test suite for AuthState decoder

AuthState can be either:
- SignedOut (no user data)
- SignedIn User (contains user data)

The JSON from JavaScript looks like:
- {"tag": "SignedOut"}
- {"tag": "SignedIn", "id": "123", "email": "user@example.com"}
-}
authStateDecoderTests : Test
authStateDecoderTests =
    describe "AuthState decoder"
        [ test "decodes SignedOut state" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "tag": "SignedOut"
                        }
                        """
                in
                Decode.decodeString authStateDecoder json
                    |> Expect.equal (Ok SignedOut)
        , test "decodes SignedIn state with user" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "tag": "SignedIn",
                            "id": "user-456",
                            "email": "hello@example.com"
                        }
                        """
                in
                Decode.decodeString authStateDecoder json
                    |> Expect.equal
                        (Ok
                            (SignedIn
                                { id = "user-456"
                                , email = "hello@example.com"
                                }
                            )
                        )
        , test "fails on unknown tag" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "tag": "UnknownState"
                        }
                        """
                in
                Decode.decodeString authStateDecoder json
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails on missing tag field" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "id": "user-123",
                            "email": "test@example.com"
                        }
                        """
                in
                Decode.decodeString authStateDecoder json
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
