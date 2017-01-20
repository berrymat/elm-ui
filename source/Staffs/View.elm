module Staffs.View exposing (..)

import Staffs.Models exposing (..)
import Html exposing (..)
import Helpers.Helpers exposing (..)
import Header.Utils exposing (headerItem)


headerItems : Staff -> List (Html msg)
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
