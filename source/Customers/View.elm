module Customers.View exposing (..)

import Customers.Models exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Helpers.Helpers exposing (..)
import Header.Utils exposing (headerItem)
import Helpers.Models exposing (..)
import Ui
import Ui.DropdownMenu
import Ui.IconButton
import Customers.Actions.View as Actions
import Customers.Actions.Models exposing (ModalType(..))


headerItems : Model -> List (Html msg)
headerItems model =
    let
        access =
            model.access

        values =
            model.customer

        address =
            fullAddress values.address1 values.address2 values.address3 values.address4 values.postcode
    in
        [ headerItem "Name" "globe" access.name values.name
        , headerItem "Address" "home" access.address address
        , headerItem "Contact" "user-o" access.contact values.contact
        , headerItem "Phone" "phone" access.contact values.tel
        , headerItem "Email" "envelope" access.contact values.email
        ]



-- ACTION DROPDOWN


dropdownMenuItem : AuthToken -> String -> String -> ModalType -> Html Msg
dropdownMenuItem token icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction token type_) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


accessibleActions : Model -> List ( String, String, ModalType )
accessibleActions model =
    let
        actions =
            [ ( "plus", "New Customer", NewCustomer )
            , ( "record", "Edit Customer", EditCustomer )
            , ( "record", "Delete Customer", DeleteCustomer )
            ]

        actionFilter ( _, _, type_ ) =
            case type_ of
                NewCustomer ->
                    -- TODO model.canAdd
                    True

                EditCustomer ->
                    -- TODO model.canEdit
                    True

                DeleteCustomer ->
                    -- TODO model.canDelete
                    True
    in
        List.filter actionFilter actions


actionDropdownViewModel : List ( String, String, ModalType ) -> AuthToken -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel actions token =
    { element =
        Ui.IconButton.secondary "Customer Actions"
            "chevron-up"
            "right"
            NoAction
    , items =
        List.map (\( icon, name, type_ ) -> dropdownMenuItem token icon name type_) actions
    }


viewActionMenu : AuthToken -> Model -> Html Msg
viewActionMenu token model =
    let
        actions =
            accessibleActions model
    in
        case actions of
            [] ->
                div [] []

            _ ->
                div []
                    [ Ui.DropdownMenu.view (actionDropdownViewModel actions token)
                        ActionMenu
                        model.actionMenu
                    , Html.map ActionsMsg (Actions.view token model.actions)
                    ]
