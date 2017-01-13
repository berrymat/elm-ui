module Users.Manager.User exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , isAdministrator : Bool
    , accessToCases : Bool
    , accessToMobiles : Bool
    , accessToStaff : Bool
    , accessToClients : Bool
    , checked : Bool
    }


userDecoder : Decode.Decoder User
userDecoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "isAdministrator" Decode.bool
        |> required "accessToCases" Decode.bool
        |> required "accessToMobiles" Decode.bool
        |> required "accessToStaff" Decode.bool
        |> required "accessToClients" Decode.bool
        |> hardcoded False


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "email", Encode.string user.email )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "isAdministrator", Encode.bool user.isAdministrator )
        , ( "accessToCases", Encode.bool user.accessToCases )
        , ( "accessToMobiles", Encode.bool user.accessToMobiles )
        , ( "accessToStaff", Encode.bool user.accessToStaff )
        , ( "accessToClients", Encode.bool user.accessToClients )
        ]
