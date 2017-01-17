module Issues.Update exposing (..)

import Issues.Models exposing (..)
import Table
import Ui.DropdownMenu
import Return exposing (..)
import Helpers.Models exposing (..)
import Issues.Issue exposing (..)
import Issues.Actions.Out exposing (..)
import Issues.Actions.Models as Actions exposing (ModalType(..))
import Issues.Actions.Update as ActionsUpdate


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        issue =
            List.filter .checked model.issues
                |> List.head
                |> Maybe.withDefault (initIssue model.id)
    in
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
                ( model, Cmd.none )

            ModalAction token modalType ->
                updateModalAction token model modalType issue

            ActionsMsg actionsMsg ->
                updateActionsMsg model actionsMsg


updateModalAction : AuthToken -> Model -> ModalType -> Issue -> Return Msg Model
updateModalAction token model modalType issue =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }

        newIssue =
            if modalType == NewIssue then
                (initIssue model.id)
            else
                issue

        ( return, out ) =
            ActionsUpdate.update (Actions.Open modalType model.sites newIssue) model.actions
    in
        return
            |> mapBoth ActionsMsg (\na -> { newModel | actions = na })


updateActionsMsg : Model -> Actions.Msg -> Return Msg Model
updateActionsMsg model actionsMsg =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )

        ( return, out ) =
            ActionsUpdate.update actionsMsg model.actions
                |> mapBothEx ActionsMsg (\na -> { model | actions = na })

        newReturn =
            case out of
                OutCancel ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel })

                OutNone ->
                    return

                OutUpdate issue ->
                    let
                        newIssues model =
                            issue :: (List.filter (\u -> u.id /= issue.id) model.issues)
                    in
                        return |> Return.map (\m -> { m | actions = Actions.NoModel, issues = newIssues m })
    in
        newReturn


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)


updateSetQuery : Model -> String -> Return Msg Model
updateSetQuery model newQuery =
    ( { model | query = newQuery }, Cmd.none )


updateSetTableState : Model -> Table.State -> Return Msg Model
updateSetTableState model newState =
    ( { model | tableState = newState }, Cmd.none )


updateToggleIssue : Model -> NodeId -> Return Msg Model
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
        ( { model | issues = newIssues }, Cmd.none )



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
