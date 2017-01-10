module Users.Update exposing (..)

import Users.Models exposing (..)
import Table
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Return exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Components.Form as Form
import Ui.Helpers.Emitter
import Http exposing (..)


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        errorCmd =
            case msg of
                UserSaveResponse webdata ->
                    Helpers.Helpers.errorCmd webdata

                _ ->
                    Cmd.none

        return =
            updateInner msg model
    in
        (return |> Return.command errorCmd)


updateInner : Msg -> Model -> Return Msg Model
updateInner msg model =
    let
        user =
            List.filter .checked model.users
                |> List.head
                |> Maybe.withDefault (initUser model.id)
    in
        case msg of
            SetQuery newQuery ->
                updateSetQuery model newQuery

            SetTableState newState ->
                updateSetTableState model newState

            ToggleUser nodeId ->
                updateToggleUser model nodeId

            ActionMenu action ->
                updateActionMenu model action

            CloseActionMenu ->
                updateCloseActionMenu model

            NoAction ->
                ( model, Cmd.none )

            -- NEW USER MODAL
            ModalAction token NewUser action ->
                updateModalActionUser token model action (initUser model.id) Post

            ModalMsg NewUser modalMsg ->
                updateModalMsgUser model modalMsg

            -- EDIT USER MODAL
            ModalAction token EditUser action ->
                updateModalActionUser token model action user Put

            ModalMsg EditUser modalMsg ->
                updateModalMsgUser model modalMsg

            UserFormMsg msg ->
                let
                    ( newUserEditModal, effect ) =
                        maybeUpdate (Form.update msg) model.userEditForm
                in
                    ( { model | userEditForm = newUserEditModal }
                    , Cmd.map UserFormMsg effect
                    )

            UserSaveResponse webdata ->
                handleWebDataResponse model webdata "Users updated" singleton

            -- DELETE USER MODAL
            ModalAction token DeleteUser action ->
                updateModalActionDeleteUser token model action

            ModalMsg DeleteUser modalMsg ->
                updateModalMsgDeleteUser model modalMsg

            -- RESET PASSWORD USER MODAL
            ModalAction token ResetPasswordUser action ->
                updateModalActionResetPasswordUser token model action

            ModalMsg ResetPasswordUser modalMsg ->
                updateModalMsgResetPasswordUser model modalMsg

            -- CHANGE PASSWORD USER MODAL
            ModalAction token ChangePasswordUser action ->
                updateModalActionChangePasswordUser token model action

            ModalMsg ChangePasswordUser modalMsg ->
                updateModalMsgChangePasswordUser model modalMsg

            UserChangePasswordFormMsg msg ->
                let
                    ( newUserChangePasswordForm, effect ) =
                        maybeUpdate (Form.update msg) model.userChangePasswordForm
                in
                    ( { model | userChangePasswordForm = newUserChangePasswordForm }
                    , Cmd.map UserChangePasswordFormMsg effect
                    )

            -- OTHER MODALS - TEMP - TODO
            ModalAction _ _ _ ->
                singleton model

            ModalMsg _ _ ->
                singleton model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.usersActionMenu)
    in
        Sub.batch
            [ subActionMenu
            ]


updateSetQuery : Model -> String -> Return Msg Model
updateSetQuery model newQuery =
    ( { model | query = newQuery }, Cmd.none )


updateSetTableState : Model -> Table.State -> Return Msg Model
updateSetTableState model newState =
    ( { model | tableState = newState }, Cmd.none )


updateToggleUser : Model -> NodeId -> Return Msg Model
updateToggleUser model nodeId =
    let
        newUsers =
            List.map
                (\u ->
                    if (u.id == nodeId || u.checked) then
                        { u | checked = not u.checked }
                    else
                        u
                )
                model.users
    in
        ( { model | users = newUsers }, Cmd.none )



-- ACTION MENU UPDATES


applyNewActionMenu : Model -> Ui.DropdownMenu.Model -> Return Msg Model
applyNewActionMenu model newMenu =
    ( { model | usersActionMenu = newMenu }, Cmd.none )


updateActionMenu : Model -> Ui.DropdownMenu.Msg -> Return Msg Model
updateActionMenu model action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action model.usersActionMenu
    in
        applyNewActionMenu model newActionMenu


updateCloseActionMenu : Model -> Return Msg Model
updateCloseActionMenu model =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.usersActionMenu
    in
        applyNewActionMenu model newActionMenu



-- NEW FOLDER UPDATES


updateModalActionUser : AuthToken -> Model -> ModalAction -> User -> HttpMethod -> Return Msg Model
updateModalActionUser token model action user method =
    case action of
        Open ->
            updateUserModalOpen model user method

        Save ->
            case model.userEditForm of
                Just form ->
                    updateUserModalSave token model user form method

                Nothing ->
                    singleton model

        Cancel ->
            ( { model | userEditModal = Ui.Modal.close model.userEditModal }, Cmd.none )


updateUserModalOpen : Model -> User -> HttpMethod -> Return Msg Model
updateUserModalOpen model user method =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.usersActionMenu

        newUserForm =
            Users.Models.userForm user
    in
        ( { model
            | usersActionMenu = newActionMenu
            , userEditMethod = Just method
            , userEditModal = Ui.Modal.open model.userEditModal
            , userEditForm = Just newUserForm
          }
        , Cmd.none
        )


updateUserModalSave : AuthToken -> Model -> User -> Form.Model -> HttpMethod -> Return Msg Model
updateUserModalSave token model user form method =
    let
        newUserEditModal =
            Ui.Modal.close model.userEditModal

        newUser =
            Users.Models.updateUser form user

        newUsers =
            List.map
                (\u ->
                    if u.id == newUser.id then
                        newUser
                    else
                        u
                )
                model.users

        newEffect =
            saveUser token newUser method
    in
        ( { model | userEditModal = newUserEditModal, users = newUsers }, newEffect )


updateModalMsgUser : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsgUser model modalMsg =
    let
        newUserEditModal =
            Ui.Modal.update modalMsg model.userEditModal
    in
        ( { model | userEditModal = newUserEditModal }, Cmd.none )



-- DELETE USER


updateModalActionDeleteUser : AuthToken -> Model -> ModalAction -> Return Msg Model
updateModalActionDeleteUser token model action =
    let
        maybeUser =
            List.filter .checked model.users
                |> List.head

        dispatch user =
            case action of
                Open ->
                    updateDeleteUserModalOpen model user

                Save ->
                    updateDeleteUserModalSave token model user

                Cancel ->
                    ( { model | userDeleteModal = Ui.Modal.close model.userDeleteModal }, Cmd.none )
    in
        maybeUser
            |> Maybe.map dispatch
            |> Maybe.withDefault (singleton model)


updateDeleteUserModalOpen : Model -> User -> Return Msg Model
updateDeleteUserModalOpen model user =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.usersActionMenu
    in
        ( { model
            | usersActionMenu = newActionMenu
            , userDeleteModal = Ui.Modal.open model.userDeleteModal
          }
        , Cmd.none
        )


updateDeleteUserModalSave : AuthToken -> Model -> User -> Return Msg Model
updateDeleteUserModalSave token model user =
    let
        newUserDeleteModal =
            Ui.Modal.close model.userDeleteModal

        newEffect =
            Helpers.Helpers.requester token "Users" user.id Delete (encodeUser user) modelDecoder (UserSaveResponse << RemoteData.fromResult)
    in
        ( { model
            | userDeleteModal = newUserDeleteModal
          }
        , newEffect
        )


updateModalMsgDeleteUser : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsgDeleteUser model modalMsg =
    let
        newUserDeleteModal =
            Ui.Modal.update modalMsg model.userDeleteModal
    in
        ( { model | userDeleteModal = newUserDeleteModal }, Cmd.none )



-- RESET PASSWORD USER


updateModalActionResetPasswordUser : AuthToken -> Model -> ModalAction -> Return Msg Model
updateModalActionResetPasswordUser token model action =
    let
        maybeUser =
            List.filter .checked model.users
                |> List.head

        dispatch user =
            case action of
                Open ->
                    updateResetPasswordUserModalOpen model user

                Save ->
                    updateResetPasswordUserModalSave token model user

                Cancel ->
                    ( { model | userResetPasswordModal = Ui.Modal.close model.userResetPasswordModal }, Cmd.none )
    in
        maybeUser
            |> Maybe.map dispatch
            |> Maybe.withDefault (singleton model)


updateResetPasswordUserModalOpen : Model -> User -> Return Msg Model
updateResetPasswordUserModalOpen model user =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.usersActionMenu
    in
        ( { model
            | usersActionMenu = newActionMenu
            , userResetPasswordModal = Ui.Modal.open model.userResetPasswordModal
          }
        , Cmd.none
        )


updateResetPasswordUserModalSave : AuthToken -> Model -> User -> Return Msg Model
updateResetPasswordUserModalSave token model user =
    let
        newUserResetPasswordModal =
            Ui.Modal.close model.userResetPasswordModal

        newEffect =
            Helpers.Helpers.requester token "ResetPassword" user.id Put (encodeUser user) modelDecoder (UserSaveResponse << RemoteData.fromResult)
    in
        ( { model
            | userResetPasswordModal = newUserResetPasswordModal
          }
        , newEffect
        )


updateModalMsgResetPasswordUser : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsgResetPasswordUser model modalMsg =
    let
        newUserResetPasswordModal =
            Ui.Modal.update modalMsg model.userResetPasswordModal
    in
        ( { model | userResetPasswordModal = newUserResetPasswordModal }, Cmd.none )



-- CHANGE PASSWORD USER


updateModalActionChangePasswordUser : AuthToken -> Model -> ModalAction -> Return Msg Model
updateModalActionChangePasswordUser token model action =
    let
        maybeUser =
            List.filter .checked model.users
                |> List.head

        dispatch user =
            case action of
                Open ->
                    updateChangePasswordUserModalOpen model user

                Save ->
                    case model.userChangePasswordForm of
                        Just form ->
                            updateChangePasswordUserModalSave token model user form

                        Nothing ->
                            singleton model

                Cancel ->
                    ( { model | userChangePasswordModal = Ui.Modal.close model.userChangePasswordModal }, Cmd.none )
    in
        maybeUser
            |> Maybe.map dispatch
            |> Maybe.withDefault (singleton model)


updateChangePasswordUserModalOpen : Model -> User -> Return Msg Model
updateChangePasswordUserModalOpen model user =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.usersActionMenu

        newUserChangePasswordForm =
            Users.Models.changePasswordForm (initChangePassword user.id)
    in
        ( { model
            | usersActionMenu = newActionMenu
            , userChangePasswordModal = Ui.Modal.open model.userChangePasswordModal
            , userChangePasswordForm = Just newUserChangePasswordForm
          }
        , Cmd.none
        )


updateChangePasswordUserModalSave : AuthToken -> Model -> User -> Form.Model -> Return Msg Model
updateChangePasswordUserModalSave token model user form =
    let
        newUserChangePasswordModal =
            Ui.Modal.close model.userChangePasswordModal

        changePassword =
            Users.Models.updateChangePassword form (initChangePassword user.id)

        newEffect =
            Helpers.Helpers.requester token "ChangePassword" user.id Put (encodeChangePassword changePassword) modelDecoder (UserSaveResponse << RemoteData.fromResult)
    in
        ( { model
            | userChangePasswordModal = newUserChangePasswordModal
          }
        , newEffect
        )


updateModalMsgChangePasswordUser : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsgChangePasswordUser model modalMsg =
    let
        newUserChangePasswordModal =
            Ui.Modal.update modalMsg model.userChangePasswordModal
    in
        ( { model | userChangePasswordModal = newUserChangePasswordModal }, Cmd.none )
