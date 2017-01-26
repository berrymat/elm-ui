module Sites.Update exposing (..)

import Sites.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Ui.DropdownMenu
import Sites.Site exposing (..)
import Sites.Actions.Out exposing (..)
import Sites.Actions.Models as Actions exposing (ModalType(..))
import Sites.Actions.Update as ActionsUpdate


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
            updateModalAction token model modalType model.site

        ActionsMsg actionsMsg ->
            updateActionsMsg model actionsMsg


updateModalAction : AuthToken -> Model -> ModalType -> Site -> Return Msg Model
updateModalAction token model modalType site =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu

        newModel =
            { model | actionMenu = newActionMenu }

        newSite =
            if modalType == NewSite then
                (initSite model.id)
            else
                site

        ( return, out ) =
            ActionsUpdate.update (Actions.Open modalType newSite) model.actions
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

                OutUpdate site ->
                    return |> Return.map (\m -> { m | actions = Actions.NoModel, site = site })

                OutDelete site ->
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
