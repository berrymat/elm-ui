module Header.Site.View exposing (..)

import Header.Site.Models exposing (..)
import Html exposing (..)
import Helpers.Helpers exposing (..)
import Header.Utils exposing (headerItem)


headerItems : Site -> List (Html msg)
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
