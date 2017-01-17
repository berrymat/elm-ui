module Roots.Root exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Root =
    { id : String
    , name : String
    , access : Bool
    , checked : Bool
    }


rootDecoder : Decode.Decoder Root
rootDecoder =
    decode Root
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "access" Decode.bool
        |> hardcoded False


encodeRoot : Root -> Encode.Value
encodeRoot root =
    Encode.object
        [ ( "id", Encode.string root.id )
        , ( "name", Encode.string root.name )
        , ( "access", Encode.bool root.access )
        ]
