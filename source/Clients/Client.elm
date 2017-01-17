module Clients.Client exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Client =
    { id : String
    , name : String
    , access : Bool
    , checked : Bool
    }


clientDecoder : Decode.Decoder Client
clientDecoder =
    decode Client
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "access" Decode.bool
        |> hardcoded False


encodeClient : Client -> Encode.Value
encodeClient client =
    Encode.object
        [ ( "id", Encode.string client.id )
        , ( "name", Encode.string client.name )
        , ( "access", Encode.bool client.access )
        ]
