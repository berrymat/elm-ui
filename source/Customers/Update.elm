module Customers.Update exposing (..)

import Customers.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Ui.DropdownMenu
import Customers.Customer exposing (..)
import Container.Out exposing (..)
import Customers.Actions.Models as Actions exposing (ModalType(..))
import Customers.Actions.Update as ActionsUpdate


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
            updateOpenModal token model modalType model.customer

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateOpenModal : AuthToken -> Model -> ModalType -> Customer -> ReturnOut Msg OutMsg Model
updateOpenModal token model modalType customer =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }
    in
        ActionsUpdate.update (Actions.Open modalType customer) model.actions
            |> mapBoth ActionsMsg (\na -> { newModel | actions = na })


updateActionsMsg : Model -> Actions.Msg -> ReturnOut Msg OutMsg Model
updateActionsMsg model actionsMsg =
    let
        applyOut out return =
            case out of
                OutUpdateCustomer method customer ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel, customer = customer })

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
