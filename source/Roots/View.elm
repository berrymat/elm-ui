module Roots.View exposing (..)

import Roots.Models exposing (..)
import Html exposing (..)
import Helpers.Helpers exposing (..)
import Header.Utils exposing (headerItem)


headerItems : Model -> List (Html msg)
headerItems model =
    let
        access =
            model.access

        values =
            model.root

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Contact" "user-o" access.contact values.contact
        , headerItem "Phone" "phone" access.contact values.tel
        , headerItem "Email" "envelope" access.contact values.email
        ]
