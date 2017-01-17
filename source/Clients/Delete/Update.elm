module Clients.Delete.Update exposing (..)

import Clients.Delete.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Ui.Modal
import Clients.Actions.Out exposing (..)
import Clients.Client exposing (..)


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    case msg of
        Save authToken ->
            updateSave model authToken

        Cancel ->
            updateCancel model

        SaveResponse response ->
            updateSaveResponse model response

        ModalMsg modalMsg ->
            updateModalMsg model modalMsg


updateSave : Model -> AuthToken -> ( Return Msg Model, OutMsg )
updateSave model authToken =
    let
        saveCmd =
            Helpers.Helpers.requester authToken
                "Clients"
                model.id
                Delete
                (encodeClient model.client)
                clientDecoder
                (SaveResponse << RemoteData.fromResult)
    in
        ( return model saveCmd, OutNone )


updateCancel : Model -> ( Return Msg Model, OutMsg )
updateCancel model =
    ( singleton { model | modal = Ui.Modal.close model.modal }, OutCancel )


updateModalMsg : Model -> Ui.Modal.Msg -> ( Return Msg Model, OutMsg )
updateModalMsg model modalMsg =
    ( singleton { model | modal = Ui.Modal.update modalMsg model.modal }, OutNone )


updateSaveResponse : Model -> WebData Client -> ( Return Msg Model, OutMsg )
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        updateSaveResponseSuccess client =
            ( singleton { newModel | modal = Ui.Modal.close model.modal }
            , OutDelete client
            )
    in
        RemoteData.map updateSaveResponseSuccess response
            |> RemoteData.withDefault ( return newModel cmd, OutNone )
