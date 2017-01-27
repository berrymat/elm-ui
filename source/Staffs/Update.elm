module Staffs.Update exposing (..)

import Staffs.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Ui.DropdownMenu
import Staffs.Staff exposing (..)
import Container.Out exposing (..)
import Staffs.Actions.Models as Actions exposing (ModalType(..))
import Staffs.Actions.Update as ActionsUpdate


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
            updateModalAction token model modalType model.staff

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateModalAction : AuthToken -> Model -> ModalType -> Staff -> Return Msg Model
updateModalAction token model modalType staff =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }

        newStaff =
            if modalType == NewStaff then
                (initStaff model.id)
            else
                staff

        ( return, out ) =
            ActionsUpdate.update (Actions.Open modalType newStaff) model.actions
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
                OutUpdateStaff method staff ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel, staff = staff })

                OutNone ->
                    return

                _ ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel })
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
