module Header.Site.View exposing (..)

import Html exposing (..)
import Container.Messages exposing (..)
import Helpers.Models exposing (..)
import Header.Utils exposing (..)
import Helpers.Helpers exposing (..)
import Components.Form as Form


headerItems : Site -> List (Html Msg)
headerItems site =
    let
        access =
            site.access

        values =
            site.values

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Ref" "wrench" access.name values.no
        , headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Contact" "user-o" access.contact values.contact
        , headerItem "Phone" "phone" access.contact values.tel
        , headerItem "Email" "envelope" access.contact values.email
        , headerItem "Division Mgr" "user-o" access.managers values.divisionMgr
        , headerItem "Area Mgr" "user-o" access.managers values.areaMgr
        , headerItem "Supervisor" "user-o" access.managers values.supervisor
        ]


initEditForm : Site -> Form.Model Msg
initEditForm site =
    let
        values =
            site.values
    in
        Form.init
            { checkboxes = []
            , inputs =
                [ ( "name", 0, "Name", (Maybe.withDefault "" values.name) )
                , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" values.address1) )
                , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" values.address2) )
                , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" values.address3) )
                , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" values.address4) )
                , ( "postcode", 5, "Postcode", (Maybe.withDefault "" values.postcode) )
                , ( "contact", 6, "Contact", (Maybe.withDefault "" values.contact) )
                , ( "phone", 7, "Phone", (Maybe.withDefault "" values.tel) )
                , ( "email", 8, "Email", (Maybe.withDefault "" values.email) )
                ]
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }


updateState : Form.Model Msg -> Site -> Site
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
