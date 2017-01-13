module Users.Restrict.Update exposing (..)

import Users.Restrict.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Table
import Ui.Modal
import Users.Manager.Out exposing (..)
import Users.Manager.User exposing (..)


update : Msg -> Model -> ( Return Msg Model, OutMsg )
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


updateSave : Model -> AuthToken -> ( Return Msg Model, OutMsg )
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
        ( return newModel cmd, OutNone )


updateCancel : Model -> ( Return Msg Model, OutMsg )
updateCancel model =
    ( singleton { model | modal = Ui.Modal.close model.modal }, OutNone )


updateRestrictResponse : Model -> WebData (List Restriction) -> ( Return Msg Model, OutMsg )
updateRestrictResponse model response =
    ( singleton { model | restrictions = response }, OutNone )


updateSetQuery : Model -> String -> ( Return Msg Model, OutMsg )
updateSetQuery model query =
    ( singleton { model | query = query }, OutNone )


updateSetTableState : Model -> Table.State -> ( Return Msg Model, OutMsg )
updateSetTableState model tableState =
    ( singleton { model | tableState = tableState }, OutNone )


updateModalMsg : Model -> Ui.Modal.Msg -> ( Return Msg Model, OutMsg )
updateModalMsg model modalMsg =
    ( singleton { model | modal = Ui.Modal.update modalMsg model.modal }, OutNone )


updateSaveResponse : Model -> WebData User -> ( Return Msg Model, OutMsg )
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        updateSaveResponseSuccess user =
            ( singleton { newModel | modal = Ui.Modal.close model.modal }
            , OutUpdate user
            )
    in
        RemoteData.map updateSaveResponseSuccess response
            |> RemoteData.withDefault ( return newModel cmd, OutNone )


updateToggleRestriction : Model -> NodeId -> ( Return Msg Model, OutMsg )
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
        ( singleton { model | restrictions = newRestrictions }, OutNone )
