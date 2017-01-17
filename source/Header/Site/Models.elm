module Header.Site.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Components.Validators exposing (..)


type alias Site =
    { id : NodeId
    , access : SiteAccess
    , values : SiteValues
    }


type alias SiteValues =
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
    , divisionMgr : Maybe String
    , areaMgr : Maybe String
    , supervisor : Maybe String
    }


type alias SiteAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    , managers : AccessType
    }


initEditForm : Site -> Form.Model
initEditForm site =
    let
        values =
            site.values
    in
        Form.init
            { checkboxes = []
            , inputs =
                [ ( "name", 0, "Name", (Maybe.withDefault "" values.name), Nothing, [ Form.Validator requiredInput ] )
                , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" values.address1), Nothing, [] )
                , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" values.address2), Nothing, [] )
                , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" values.address3), Nothing, [] )
                , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" values.address4), Nothing, [] )
                , ( "postcode", 5, "Postcode", (Maybe.withDefault "" values.postcode), Nothing, [] )
                , ( "contact", 6, "Contact", (Maybe.withDefault "" values.contact), Nothing, [] )
                , ( "phone", 7, "Phone", (Maybe.withDefault "" values.tel), Nothing, [] )
                , ( "email", 8, "Email", (Maybe.withDefault "" values.email), Nothing, [ Form.Validator optionalValidEmail ] )
                ]
            , fileInputs = []
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }


updateState : Form.Model -> Site -> Site
updateState form site =
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
        { site | values = updatedValues site.values }


encodeSite : Site -> Encode.Value
encodeSite site =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" site.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" site.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" site.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" site.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" site.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" site.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" site.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" site.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" site.values.email) )
                ]
          )
        ]


siteAccessDecoder : Decode.Decoder SiteAccess
siteAccessDecoder =
    decode createSiteAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string
        |> required "managers" Decode.string


createSiteAccess : String -> String -> String -> String -> String -> SiteAccess
createSiteAccess name image address contact managers =
    SiteAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)
        (convertAccessType managers)


siteValuesDecoder : Decode.Decoder SiteValues
siteValuesDecoder =
    decode SiteValues
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
