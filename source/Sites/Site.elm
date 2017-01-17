module Sites.Site exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Site =
    { id : String
    , name : String
    , access : Bool
    , checked : Bool
    }


siteDecoder : Decode.Decoder Site
siteDecoder =
    decode Site
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "access" Decode.bool
        |> hardcoded False


encodeSite : Site -> Encode.Value
encodeSite site =
    Encode.object
        [ ( "id", Encode.string site.id )
        , ( "name", Encode.string site.name )
        , ( "access", Encode.bool site.access )
        ]
