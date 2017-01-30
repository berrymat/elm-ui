module Users.Update exposing (..)

import Users.Models exposing (..)
import Table
import Ui.DropdownMenu
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Users.User exposing (..)
import Container.Out exposing (..)
import Users.Actions.Models as Actions exposing (ModalType(..))
import Users.Actions.Update as ActionsUpdate


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
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
            singleton model

        -- NEW USER MODAL
        OpenModal token NewUser ->
            (initUser model.id)
                |> updateOpenModal token model NewUser

        OpenModal token modalType ->
            List.filter .checked model.users
                |> List.head
                |> Maybe.map (updateOpenModal token model modalType)
                |> Maybe.withDefault (singleton model)

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateOpenModal : AuthToken -> Model -> ModalType -> User -> ReturnOut Msg OutMsg Model
updateOpenModal token model modalType user =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }
    in
        ActionsUpdate.update (Actions.Open modalType user) model.actions
            |> mapBoth ActionsMsg (\na -> { newModel | actions = na })


updateActionsMsg : Model -> Actions.Msg -> ReturnOut Msg OutMsg Model
updateActionsMsg model actionsMsg =
    let
        applyOut out return =
            case out of
                OutUpdateUser Post user ->
                    let
                        newIssues model =
                            user :: model.users
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, users = newIssues m })

                OutUpdateUser Put user ->
                    let
                        newIssues model =
                            List.map
                                (\u ->
                                    if u.id == user.id then
                                        user
                                    else
                                        u
                                )
                                model.users
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, users = newIssues m })

                _ ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel })
    in
        ActionsUpdate.update actionsMsg model.actions
            |> mapBoth ActionsMsg (\na -> { model | actions = na })
            |> mapOut applyOut


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)


updateSetQuery : Model -> String -> ReturnOut Msg OutMsg Model
updateSetQuery model newQuery =
    singleton { model | query = newQuery }


updateSetTableState : Model -> Table.State -> ReturnOut Msg OutMsg Model
updateSetTableState model newState =
    singleton { model | tableState = newState }


updateToggleUser : Model -> NodeId -> ReturnOut Msg OutMsg Model
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
        singleton { model | users = newUsers }



-- ACTION MENU UPDATES


applyNewActionMenu : Model -> Ui.DropdownMenu.Model -> ReturnOut Msg OutMsg Model
applyNewActionMenu model newMenu =
    singleton { model | actionMenu = newMenu }


updateActionMenu : Model -> Ui.DropdownMenu.Msg -> ReturnOut Msg OutMsg Model
updateActionMenu model action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action model.actionMenu
    in
        applyNewActionMenu model newActionMenu


updateCloseActionMenu : Model -> ReturnOut Msg OutMsg Model
updateCloseActionMenu model =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu
    in
        applyNewActionMenu model newActionMenu
