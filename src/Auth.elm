module Auth exposing
    ( AuthState(..)
    , LoginFormState
    , User
    , authStateDecoder
    , isValidEmail
    , userDecoder
    )

{-| Authentication module

This module handles user authentication state and validation.

-}

import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..))


{-| User represents an authenticated user
-}
type alias User =
    { id : String
    , email : String
    , accessToken : String  -- JWT token for authenticated requests
    }


{-| AuthState represents whether a user is signed in or out
-}
type AuthState
    = SignedOut
    | SignedIn User


{-| LoginFormState tracks the state of the login modal form
-}
type alias LoginFormState =
    { email : String
    , status : RemoteData String ()
    }


{-| Simple email validation

We check for:

  - Length > 3 (minimum like "a@b.c")
  - Contains exactly one @
  - Has characters before and after @

This is intentionally simple - we rely on Supabase to do full validation.
The goal is just to prevent obvious mistakes and provide good UX.

-}
isValidEmail : String -> Bool
isValidEmail email =
    let
        parts =
            String.split "@" email

        hasOneAt =
            List.length parts == 2

        hasBothParts =
            case parts of
                [ before, after ] ->
                    not (String.isEmpty before) && not (String.isEmpty after)

                _ ->
                    False

        longEnough =
            String.length email > 3
    in
    hasOneAt && hasBothParts && longEnough


{-| Decoder for User type from JSON

Expected JSON format:

    { "id": "user-123"
    , "email": "user@example.com"
    , "accessToken": "eyJhbGc..."
    }

-}
userDecoder : Decoder User
userDecoder =
    Decode.map3 User
        (Decode.field "id" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "accessToken" Decode.string)


{-| Decoder for AuthState from JSON

Expected JSON formats:

SignedOut:

    { "tag": "SignedOut" }

SignedIn:

    { "tag": "SignedIn"
    , "id": "user-123"
    , "email": "user@example.com"
    , "accessToken": "eyJhbGc..."
    }

-}
authStateDecoder : Decoder AuthState
authStateDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "SignedOut" ->
                        Decode.succeed SignedOut

                    "SignedIn" ->
                        Decode.map SignedIn userDecoder

                    _ ->
                        Decode.fail ("Unknown auth state tag: " ++ tag)
            )
