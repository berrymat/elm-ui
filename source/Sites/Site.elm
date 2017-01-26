module Sites.Site exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Helpers.Models exposing (..)


type alias Site =
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
    , divisionMgr : Maybe String
    , areaMgr : Maybe String
    , supervisor : Maybe String
    }


siteDecoder : Decode.Decoder Site
siteDecoder =
    decode Site
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
        |> required "divisionMgr" (Decode.nullable Decode.string)
        |> required "areaMgr" (Decode.nullable Decode.string)
        |> required "supervisor" (Decode.nullable Decode.string)


encodeSite : Site -> Encode.Value
encodeSite site =
    Encode.object
        [ ( "id", Encode.string site.id )
        , ( "name", Encode.string (Maybe.withDefault "" site.name) )
        , ( "address1", Encode.string (Maybe.withDefault "" site.address1) )
        , ( "address2", Encode.string (Maybe.withDefault "" site.address2) )
        , ( "address3", Encode.string (Maybe.withDefault "" site.address3) )
        , ( "address4", Encode.string (Maybe.withDefault "" site.address4) )
        , ( "postcode", Encode.string (Maybe.withDefault "" site.postcode) )
        , ( "contact", Encode.string (Maybe.withDefault "" site.contact) )
        , ( "tel", Encode.string (Maybe.withDefault "" site.tel) )
        , ( "email", Encode.string (Maybe.withDefault "" site.email) )
        ]
