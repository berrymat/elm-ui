module Customers.Customer exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Customer =
    { id : String
    , name : String
    , access : Bool
    , checked : Bool
    }


customerDecoder : Decode.Decoder Customer
customerDecoder =
    decode Customer
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "access" Decode.bool
        |> hardcoded False


encodeCustomer : Customer -> Encode.Value
encodeCustomer customer =
    Encode.object
        [ ( "id", Encode.string customer.id )
        , ( "name", Encode.string customer.name )
        , ( "access", Encode.bool customer.access )
        ]
