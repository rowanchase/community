module RemoteData exposing (RemoteData(..))

{-| RemoteData type for tracking async operations

This type represents the state of data that needs to be fetched from a remote source.
It's a common pattern in Elm for handling HTTP requests and other async operations.

    - NotAsked: We haven't requested the data yet
    - Loading: Request is in flight
    - Success data: Request succeeded, here's the data
    - Failure error: Request failed, here's the error

-}


type RemoteData error data
    = NotAsked
    | Loading
    | Success data
    | Failure error
