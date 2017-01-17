module Staffs.Staff exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Staff =
    { id : String
    , name : String
    , access : Bool
    , checked : Bool
    }


staffDecoder : Decode.Decoder Staff
staffDecoder =
    decode Staff
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "access" Decode.bool
        |> hardcoded False


encodeStaff : Staff -> Encode.Value
encodeStaff staff =
    Encode.object
        [ ( "id", Encode.string staff.id )
        , ( "name", Encode.string staff.name )
        , ( "access", Encode.bool staff.access )
        ]
