module Users.Update exposing (..)

import Users.Models exposing (..)
import Table
import Ui.DropdownMenu
import Return exposing (..)
import Helpers.Models exposing (..)
import Users.User exposing (..)
import Users.Actions.Out exposing (..)
import Users.Actions.Models as Actions exposing (ModalType(..))
import Users.Actions.Update as ActionsUpdate


update : Msg -> Model -> Return Msg Model
update msg model =
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
            ModalAction token modalType ->
                updateModalAction token model modalType user

            ActionsMsg actionsMsg ->
                updateActionsMsg model actionsMsg


updateModalAction : AuthToken -> Model -> ModalType -> User -> Return Msg Model
updateModalAction token model modalType user =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }

        newUser =
            if modalType == NewUser then
                (initUser model.id)
            else
                user

        ( return, out ) =
            ActionsUpdate.update (Actions.Open modalType newUser) model.actions
    in
        return
            |> mapBoth ActionsMsg (\nm -> { newModel | actions = nm })


updateActionsMsg : Model -> Actions.Msg -> Return Msg Model
updateActionsMsg model actionsMsg =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )

        ( return, out ) =
            ActionsUpdate.update actionsMsg model.actions
                |> mapBothEx ActionsMsg (\nm -> { model | actions = nm })

        newReturn =
            case out of
                OutCancel ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel })

                OutNone ->
                    return

                OutUpdate user ->
                    let
                        newUsers model =
                            user :: (List.filter (\u -> u.id /= user.id) model.users)
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, users = newUsers m })

                OutDelete user ->
                    let
                        newUsers model =
                            (List.filter (\u -> u.id /= user.id) model.users)
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, users = newUsers m })
    in
        newReturn


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)
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
    ( { model | actionMenu = newMenu }, Cmd.none )


updateActionMenu : Model -> Ui.DropdownMenu.Msg -> Return Msg Model
updateActionMenu model action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action model.actionMenu
    in
        applyNewActionMenu model newActionMenu


updateCloseActionMenu : Model -> Return Msg Model
updateCloseActionMenu model =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu
    in
        applyNewActionMenu model newActionMenu
