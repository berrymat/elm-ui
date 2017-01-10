module Header.Staff.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type alias Staff =
    { id : NodeId
    , access : StaffAccess
    , values : StaffValues
    }


type alias StaffValues =
    { no : Maybe String
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


type alias StaffAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


initEditForm : Staff -> Form.Model
initEditForm staff =
    let
        values =
            staff.values
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
                , ( "phone", 7, "Phone", (Maybe.withDefault "" values.tel), Nothing )
                , ( "mobile", 6, "Mobile", (Maybe.withDefault "" values.mob), Nothing )
                , ( "email", 8, "Email", (Maybe.withDefault "" values.email), Nothing )
                ]
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }


updateState : Form.Model -> Staff -> Staff
updateState form staff =
    let
        updatedValues values =
            { values
                | name = Just (Form.valueOfInput "name" "" form)
                , address1 = Just (Form.valueOfInput "address1" "" form)
                , address2 = Just (Form.valueOfInput "address2" "" form)
                , address3 = Just (Form.valueOfInput "address3" "" form)
                , address4 = Just (Form.valueOfInput "address4" "" form)
                , postcode = Just (Form.valueOfInput "postcode" "" form)
                , tel = Just (Form.valueOfInput "phone" "" form)
                , mob = Just (Form.valueOfInput "mobile" "" form)
                , email = Just (Form.valueOfInput "email" "" form)
            }
    in
        { staff | values = updatedValues staff.values }


encodeStaff : Staff -> Encode.Value
encodeStaff staff =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" staff.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" staff.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" staff.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" staff.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" staff.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" staff.values.postcode) )
                , ( "tel", Encode.string (Maybe.withDefault "" staff.values.tel) )
                , ( "mob", Encode.string (Maybe.withDefault "" staff.values.mob) )
                , ( "email", Encode.string (Maybe.withDefault "" staff.values.email) )
                ]
          )
        ]


staffAccessDecoder : Decode.Decoder StaffAccess
staffAccessDecoder =
    decode createStaffAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createStaffAccess : String -> String -> String -> String -> StaffAccess
createStaffAccess name image address contact =
    StaffAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


staffValuesDecoder : Decode.Decoder StaffValues
staffValuesDecoder =
    decode StaffValues
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
