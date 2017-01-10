module Header.Client.View exposing (..)

import Header.Client.Models exposing (..)
import Html exposing (..)
import Helpers.Helpers exposing (..)
import Header.Utils exposing (headerItem)


headerItems : Client -> List (Html msg)
headerItems client =
    let
        access =
            client.access

        values =
            client.values

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Ref" "wrench" access.name values.no
        , headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Contact" "user-o" access.contact values.contact
        , headerItem "Phone" "phone" access.contact values.tel
        , headerItem "Email" "envelope" access.contact values.email
        ]
