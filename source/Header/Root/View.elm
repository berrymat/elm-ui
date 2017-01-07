module Header.Root.View exposing (..)

import Html exposing (..)
import Container.Messages exposing (..)
import Helpers.Models exposing (..)
import Header.Utils exposing (..)
import Helpers.Helpers exposing (..)
import Components.Form as Form


headerItems : Root -> List (Html Msg)
headerItems root =
    let
        access =
            root.access

        values =
            root.values

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Contact" "user-o" access.contact values.contact
        , headerItem "Phone" "phone" access.contact values.tel
        , headerItem "Email" "envelope" access.contact values.email
        ]


initEditForm : Root -> Form.Model Msg
initEditForm root =
    let
        values =
            root.values
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


updateState : Form.Model Msg -> Root -> Root
updateState form root =
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
        { root | values = updatedValues root.values }
