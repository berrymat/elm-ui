module Clients.Client exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Helpers.Models exposing (..)


type alias Client =
    { id : NodeId
    , no : Maybe String
    , name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , contact : Maybe String
    , tel : Maybe String
    , email : Maybe String
    }


clientDecoder : Decode.Decoder Client
clientDecoder =
    decode Client
        |> required "id" Decode.string
        |> required "no" (Decode.nullable Decode.string)
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "contact" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)


encodeClient : Client -> Encode.Value
encodeClient client =
    Encode.object
        [ ( "id", Encode.string client.id )
        , ( "no", Encode.string (Maybe.withDefault "" client.no) )
        , ( "name", Encode.string (Maybe.withDefault "" client.name) )
        , ( "address1", Encode.string (Maybe.withDefault "" client.address1) )
        , ( "address2", Encode.string (Maybe.withDefault "" client.address2) )
        , ( "address3", Encode.string (Maybe.withDefault "" client.address3) )
        , ( "address4", Encode.string (Maybe.withDefault "" client.address4) )
        , ( "postcode", Encode.string (Maybe.withDefault "" client.postcode) )
        , ( "contact", Encode.string (Maybe.withDefault "" client.contact) )
        , ( "tel", Encode.string (Maybe.withDefault "" client.tel) )
        , ( "email", Encode.string (Maybe.withDefault "" client.email) )
        ]
