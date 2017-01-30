module Clients.Delete.Update exposing (..)

import Clients.Delete.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Helpers.Return as Return exposing (..)
import RemoteData exposing (..)
import Ui.Modal
import Container.Out exposing (..)
import Clients.Client exposing (..)


update : Msg -> Model -> ReturnOut Msg OutMsg Model
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


updateSave : Model -> AuthToken -> ReturnOut Msg OutMsg Model
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
        return model saveCmd


updateCancel : Model -> ReturnOut Msg OutMsg Model
updateCancel model =
    out { model | modal = Ui.Modal.close model.modal }
        Cmd.none
        OutCancel


updateModalMsg : Model -> Ui.Modal.Msg -> ReturnOut Msg OutMsg Model
updateModalMsg model modalMsg =
    singleton { model | modal = Ui.Modal.update modalMsg model.modal }


updateSaveResponse : Model -> WebData Client -> ReturnOut Msg OutMsg Model
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        success value =
            out { newModel | modal = Ui.Modal.close model.modal }
                Cmd.none
                (OutDeleteClient value)
    in
        RemoteData.map success response
            |> RemoteData.withDefault (return newModel cmd)
