module Issues.Update exposing (..)

import Issues.Models exposing (..)
import Table
import Ui.DropdownMenu
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Issues.Issue exposing (..)
import Container.Out exposing (..)
import Issues.Actions.Models as Actions exposing (ModalType(..))
import Issues.Actions.Update as ActionsUpdate


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case msg of
        SetQuery newQuery ->
            updateSetQuery model newQuery

        SetTableState newState ->
            updateSetTableState model newState

        ToggleIssue nodeId ->
            updateToggleIssue model nodeId

        ActionMenu action ->
            updateActionMenu model action

        CloseActionMenu ->
            updateCloseActionMenu model

        NoAction ->
            singleton model

        OpenModal token NewIssue ->
            (initIssue model.id)
                |> updateOpenModal token model NewIssue

        OpenModal token modalType ->
            List.filter .checked model.issues
                |> List.head
                |> Maybe.map (updateOpenModal token model modalType)
                |> Maybe.withDefault (singleton model)

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateOpenModal : AuthToken -> Model -> ModalType -> Issue -> ReturnOut Msg OutMsg Model
updateOpenModal token model modalType issue =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }
    in
        ActionsUpdate.update (Actions.Open modalType model.sites issue) model.actions
            |> mapBoth ActionsMsg (\na -> { newModel | actions = na })


updateActionsMsg : Model -> Actions.Msg -> ReturnOut Msg OutMsg Model
updateActionsMsg model actionsMsg =
    let
        applyOut out return =
            case out of
                OutUpdateIssue Post issue ->
                    let
                        newIssues model =
                            issue :: model.issues
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, issues = newIssues m })

                OutUpdateIssue Put issue ->
                    let
                        newIssues model =
                            List.map
                                (\u ->
                                    if u.id == issue.id then
                                        issue
                                    else
                                        u
                                )
                                model.issues
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, issues = newIssues m })

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


updateToggleIssue : Model -> NodeId -> ReturnOut Msg OutMsg Model
updateToggleIssue model nodeId =
    let
        newIssues =
            List.map
                (\u ->
                    if (u.id == nodeId || u.checked) then
                        { u | checked = not u.checked }
                    else
                        u
                )
                model.issues
    in
        singleton { model | issues = newIssues }



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
