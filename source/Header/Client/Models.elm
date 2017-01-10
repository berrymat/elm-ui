module Header.Client.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Client =
    { id : NodeId
    , access : ClientAccess
    , values : ClientValues
    }


type alias ClientValues =
    { no : Maybe String
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


type alias ClientAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


initEditForm : Client -> Form.Model msg
initEditForm client =
    let
        values =
            client.values
    in
        Form.init
            { checkboxes = []
            , inputs =
                [ ( "name", 0, "Name", (Maybe.withDefault "" values.name), Nothing )
                , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" values.address1), Nothing )
                , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" values.address2), Nothing )
                , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" values.address3), Nothing )
                , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" values.address4), Nothing )
                , ( "postcode", 5, "Postcode", (Maybe.withDefault "" values.postcode), Nothing )
                , ( "contact", 6, "Contact", (Maybe.withDefault "" values.contact), Nothing )
                , ( "phone", 7, "Phone", (Maybe.withDefault "" values.tel), Nothing )
                , ( "email", 8, "Email", (Maybe.withDefault "" values.email), Nothing )
                ]
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }


updateState : Form.Model msg -> Client -> Client
updateState form client =
    let
        updatedValues values =
            { values
                | name = Just (Form.valueOfInput "name" "" form)
                , address1 = Just (Form.valueOfInput "address1" "" form)
                , address2 = Just (Form.valueOfInput "address2" "" form)
                , address3 = Just (Form.valueOfInput "address3" "" form)
                , address4 = Just (Form.valueOfInput "address4" "" form)
                , postcode = Just (Form.valueOfInput "postcode" "" form)
                , contact = Just (Form.valueOfInput "contact" "" form)
                , tel = Just (Form.valueOfInput "phone" "" form)
                , email = Just (Form.valueOfInput "email" "" form)
            }
    in
        { client | values = updatedValues client.values }


encodeClient : Client -> Encode.Value
encodeClient client =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" client.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" client.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" client.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" client.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" client.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" client.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" client.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" client.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" client.values.email) )
                ]
          )
        ]


clientAccessDecoder : Decode.Decoder ClientAccess
clientAccessDecoder =
    decode createClientAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createClientAccess : String -> String -> String -> String -> ClientAccess
createClientAccess name image address contact =
    ClientAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


clientValuesDecoder : Decode.Decoder ClientValues
clientValuesDecoder =
    decode ClientValues
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
