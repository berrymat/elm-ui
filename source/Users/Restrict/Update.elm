module Users.Restrict.Update exposing (..)

import Users.Restrict.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Helpers.Return as Return exposing (..)
import RemoteData exposing (..)
import Table
import Ui.Modal
import Container.Out exposing (..)
import Users.User exposing (..)


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case msg of
        Save authToken ->
            updateSave model authToken

        Cancel ->
            updateCancel model

        RestrictResponse response ->
            updateRestrictResponse model response

        SaveResponse response ->
            updateSaveResponse model response

        ToggleRestriction nodeId ->
            updateToggleRestriction model nodeId

        SetQuery query ->
            updateSetQuery model query

        SetTableState tableState ->
            updateSetTableState model tableState

        ModalMsg modalMsg ->
            updateModalMsg model modalMsg


updateSave : Model -> AuthToken -> ReturnOut Msg OutMsg Model
updateSave model authToken =
    let
        saveRestrictions authToken restrictions =
            ( restrictions
            , putter authToken
                "Restrictions"
                model.id
                (encodeRestrictions restrictions)
                userDecoder
                (SaveResponse << RemoteData.fromResult)
            )

        ( newRestrictions, cmd ) =
            RemoteData.update (saveRestrictions authToken) model.restrictions

        newModel =
            { model | restrictions = newRestrictions }
    in
        return newModel cmd


updateCancel : Model -> ReturnOut Msg OutMsg Model
updateCancel model =
    out { model | modal = Ui.Modal.close model.modal }
        Cmd.none
        OutCancel


updateRestrictResponse : Model -> WebData (List Restriction) -> ReturnOut Msg OutMsg Model
updateRestrictResponse model response =
    singleton { model | restrictions = response }


updateSetQuery : Model -> String -> ReturnOut Msg OutMsg Model
updateSetQuery model query =
    singleton { model | query = query }


updateSetTableState : Model -> Table.State -> ReturnOut Msg OutMsg Model
updateSetTableState model tableState =
    singleton { model | tableState = tableState }


updateModalMsg : Model -> Ui.Modal.Msg -> ReturnOut Msg OutMsg Model
updateModalMsg model modalMsg =
    singleton { model | modal = Ui.Modal.update modalMsg model.modal }


updateSaveResponse : Model -> WebData User -> ReturnOut Msg OutMsg Model
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        success value =
            out { newModel | modal = Ui.Modal.close model.modal }
                Cmd.none
                (OutUpdateUser Put value)
    in
        RemoteData.map success response
            |> RemoteData.withDefault (return newModel cmd)


updateToggleRestriction : Model -> NodeId -> ReturnOut Msg OutMsg Model
updateToggleRestriction model nodeId =
    let
        toggleRestrictions restrictions =
            List.map
                (\r ->
                    if r.id == nodeId then
                        { r | selected = not r.selected }
                    else
                        r
                )
                restrictions

        newRestrictions =
            RemoteData.map toggleRestrictions model.restrictions
    in
        singleton { model | restrictions = newRestrictions }
