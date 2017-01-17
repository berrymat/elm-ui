module Users.View exposing (..)

import Users.Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Table exposing (..)
import Helpers.Models exposing (..)
import Dict
import Ui
import Ui.DropdownMenu
import Ui.IconButton
import Users.Manager.User exposing (..)
import Users.Manager.View as Manager
import Users.Manager.Models exposing (ModalType(..))
import Helpers.Table exposing (..)


view : AuthToken -> Model -> Html Msg
view token model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableUsers =
            List.filter (String.contains lowerQuery << String.toLower << .email) model.users
    in
        div [ class "body-content" ]
            [ div [ class "body-content-content" ]
                [ div [ class "body-content-content-content" ]
                    [ div [ class "file-content" ]
                        [ input [ placeholder "Search by Email", onInput SetQuery ] []
                        , Table.view config model.tableState acceptableUsers
                        ]
                    ]
                , div [ class "body-content-content-footer" ]
                    [ viewActionMenu token model
                    , div [ class "flexer" ] []
                    ]
                ]
            ]


config : Table.Config User Msg
config =
    let
        columnInfo =
            Dict.fromList
                [ ( "select", ( "", "radio" ) )
                , ( "email", ( "Email", "email" ) )
                , ( "firstName", ( "First Name", "firstName" ) )
                , ( "lastName", ( "Last Name", "lastName" ) )
                ]
    in
        Table.customConfig
            { toId = .id
            , toMsg = SetTableState
            , columns =
                [ radioColumn "select" .checked .id ToggleUser
                , stringColumn "email" .email
                , stringColumn "firstName" .firstName
                , stringColumn "lastName" .lastName
                ]
            , customizations = (customizations columnInfo)
            }



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
        user =
            List.filter .checked model.users
                |> List.head

        actions =
            [ ( "plus", "New User", NewUser )
            , ( "record", "Edit User", EditUser )
            , ( "record", "Restrictions", RestrictUser )
            , ( "record", "Reset Password", ResetPasswordUser )
            , ( "record", "Change Password", ChangePasswordUser )
            , ( "trash-b", "Delete User", DeleteUser )
            ]

        hasUserAnd can =
            Maybe.map (\_ -> can) user
                |> Maybe.withDefault False

        actionFilter ( _, _, type_ ) =
            case type_ of
                NewUser ->
                    model.canAdd

                EditUser ->
                    hasUserAnd model.canEdit

                RestrictUser ->
                    hasUserAnd model.canRestrict

                ResetPasswordUser ->
                    hasUserAnd model.canResetPassword

                ChangePasswordUser ->
                    hasUserAnd model.canChangePassword

                DeleteUser ->
                    hasUserAnd model.canDelete
    in
        List.filter actionFilter actions


actionDropdownViewModel : List ( String, String, ModalType ) -> AuthToken -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel actions token =
    { element =
        Ui.IconButton.secondary "User Actions"
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

        user =
            List.filter .checked model.users
                |> List.head
                |> Maybe.withDefault (initUser model.id)
    in
        case actions of
            [] ->
                div [] []

            _ ->
                div []
                    [ Ui.DropdownMenu.view (actionDropdownViewModel actions token)
                        ActionMenu
                        model.actionMenu
                    , Html.map ManagerMsg (Manager.view token model.manager)
                    ]
