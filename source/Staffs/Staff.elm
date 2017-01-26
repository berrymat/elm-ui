module Staffs.Staff exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Helpers.Models exposing (..)


type alias Staff =
    { id : NodeId
    , no : Maybe String
    , name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , tel : Maybe String
    , mob : Maybe String
    , email : Maybe String
    }


staffDecoder : Decode.Decoder Staff
staffDecoder =
    decode Staff
        |> required "id" Decode.string
        |> required "no" (Decode.nullable Decode.string)
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "mob" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)


encodeStaff : Staff -> Encode.Value
encodeStaff staff =
    Encode.object
        [ ( "id", Encode.string staff.id )
        , ( "name", Encode.string (Maybe.withDefault "" staff.name) )
        , ( "address1", Encode.string (Maybe.withDefault "" staff.address1) )
        , ( "address2", Encode.string (Maybe.withDefault "" staff.address2) )
        , ( "address3", Encode.string (Maybe.withDefault "" staff.address3) )
        , ( "address4", Encode.string (Maybe.withDefault "" staff.address4) )
        , ( "postcode", Encode.string (Maybe.withDefault "" staff.postcode) )
        , ( "tel", Encode.string (Maybe.withDefault "" staff.tel) )
        , ( "mob", Encode.string (Maybe.withDefault "" staff.mob) )
        , ( "email", Encode.string (Maybe.withDefault "" staff.email) )
        ]
