module Customers.Update exposing (..)

import Customers.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Ui.DropdownMenu
import Customers.Customer exposing (..)
import Customers.Actions.Out exposing (..)
import Customers.Actions.Models as Actions exposing (ModalType(..))
import Customers.Actions.Update as ActionsUpdate


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ActionMenu action ->
            updateActionMenu model action

        CloseActionMenu ->
            updateCloseActionMenu model

        NoAction ->
            ( model, Cmd.none )

        ModalAction token modalType ->
            updateModalAction token model modalType model.customer

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateModalAction : AuthToken -> Model -> ModalType -> Customer -> Return Msg Model
updateModalAction token model modalType customer =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }

        newCustomer =
            if modalType == NewCustomer then
                (initCustomer model.id)
            else
                customer

        ( return, out ) =
            ActionsUpdate.update (Actions.Open modalType newCustomer) model.actions
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

                OutUpdate customer ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel, customer = customer })

                OutDelete customer ->
                    -- TODO
                    return
    in
        newReturn


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)



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
