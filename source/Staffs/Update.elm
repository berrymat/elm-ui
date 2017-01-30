module Staffs.Update exposing (..)

import Staffs.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Ui.DropdownMenu
import Staffs.Staff exposing (..)
import Container.Out exposing (..)
import Staffs.Actions.Models as Actions exposing (ModalType(..))
import Staffs.Actions.Update as ActionsUpdate


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case msg of
        ActionMenu action ->
            updateActionMenu model action

        CloseActionMenu ->
            updateCloseActionMenu model

        NoAction ->
            singleton model

        OpenModal token modalType ->
            updateOpenModal token model modalType model.staff

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateOpenModal : AuthToken -> Model -> ModalType -> Staff -> ReturnOut Msg OutMsg Model
updateOpenModal token model modalType staff =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }
    in
        ActionsUpdate.update (Actions.Open modalType staff) model.actions
            |> mapBoth ActionsMsg (\na -> { newModel | actions = na })


updateActionsMsg : Model -> Actions.Msg -> ReturnOut Msg OutMsg Model
updateActionsMsg model actionsMsg =
    let
        applyOut out return =
            case out of
                OutUpdateStaff method staff ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel, staff = staff })

                _ ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel })
    in
        ActionsUpdate.update actionsMsg model.actions
            |> mapBoth ActionsMsg (\na -> { model | actions = na })
            |> mapOut applyOut


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)



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
