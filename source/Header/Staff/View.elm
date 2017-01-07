module Header.Staff.View exposing (..)

import Html exposing (..)
import Container.Messages exposing (..)
import Helpers.Models exposing (..)
import Header.Utils exposing (..)
import Helpers.Helpers exposing (..)
import Components.Form as Form


headerItems : Staff -> List (Html Msg)
headerItems staff =
    let
        access =
            staff.access

        values =
            staff.values

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Ref" "wrench" access.name values.no
        , headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Tel" "phone" access.contact values.tel
        , headerItem "Mob" "phone" access.contact values.mob
        , headerItem "Email" "envelope" access.contact values.email
        ]


initEditForm : Staff -> Form.Model Msg
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


updateState : Form.Model Msg -> Staff -> Staff
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
