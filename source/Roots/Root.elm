module Roots.Root exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Helpers.Models exposing (..)


type alias Root =
    { id : NodeId
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


rootDecoder : Decode.Decoder Root
rootDecoder =
    decode Root
        |> required "id" Decode.string
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


encodeRoot : Root -> Encode.Value
encodeRoot root =
    Encode.object
        [ ( "id", Encode.string root.id )
        , ( "name", Encode.string (Maybe.withDefault "" root.name) )
        , ( "address1", Encode.string (Maybe.withDefault "" root.address1) )
        , ( "address2", Encode.string (Maybe.withDefault "" root.address2) )
        , ( "address3", Encode.string (Maybe.withDefault "" root.address3) )
        , ( "address4", Encode.string (Maybe.withDefault "" root.address4) )
        , ( "postcode", Encode.string (Maybe.withDefault "" root.postcode) )
        , ( "contact", Encode.string (Maybe.withDefault "" root.contact) )
        , ( "tel", Encode.string (Maybe.withDefault "" root.tel) )
        , ( "email", Encode.string (Maybe.withDefault "" root.email) )
        ]
