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
            ModalAction NewUser action ->
                updateModalActionUser model action (initUser model.id) Post

            ModalMsg NewUser modalMsg ->
                updateModalMsgUser model modalMsg

            -- EDIT USER MODAL
            ModalAction EditUser action ->
                updateModalActionUser model action user Put

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
                case webdata of
                    NotAsked ->
                        ( model, Cmd.none )

                    Loading ->
                        ( model, Cmd.none )

                    Failure err ->
                        ( model, Cmd.none )

                    Success newModel ->
                        ( newModel, Cmd.none )

            -- OTHER MODALS - TEMP - TODO
            ModalAction _ _ ->
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


updateModalActionUser : Model -> ModalAction -> User -> HttpMethod -> Return Msg Model
updateModalActionUser model action user method =
    let
        ( newModel, newCmd ) =
            case action of
                Open ->
                    updateUserModalOpen model user method

                Save ->
                    case model.userEditForm of
                        Just form ->
                            updateUserModalSave model user form method

                        Nothing ->
                            ( model, Cmd.none )

                Cancel ->
                    ( { model | userEditModal = Ui.Modal.close model.userEditModal }, Cmd.none )
    in
        ( newModel, newCmd )


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


updateUserModalSave : Model -> User -> Form.Model Msg -> HttpMethod -> Return Msg Model
updateUserModalSave model user form method =
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
            saveUser newUser method
    in
        ( { model | userEditModal = newUserEditModal, users = newUsers }, newEffect )


updateModalMsgUser : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsgUser model modalMsg =
    let
        newUserEditModal =
            Ui.Modal.update modalMsg model.userEditModal
    in
        ( { model | userEditModal = newUserEditModal }, Cmd.none )
