module Users.ChangePassword.Update exposing (..)

import Users.ChangePassword.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
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

        SaveResponse response ->
            updateSaveResponse model response

        ModalMsg modalMsg ->
            updateModalMsg model modalMsg

        FormMsg formMsg ->
            updateFormMsg model formMsg


updateSave : Model -> AuthToken -> ( Return Msg Model, OutMsg )
updateSave model authToken =
    let
        newChangePassword =
            updateChangePassword model.form model.changePassword

        saveCmd =
            Helpers.Helpers.requester authToken
                "ChangePassword"
                model.id
                Put
                (encodeChangePassword newChangePassword)
                userDecoder
                (SaveResponse << RemoteData.fromResult)
    in
        ( return model saveCmd, OutNone )


updateCancel : Model -> ( Return Msg Model, OutMsg )
updateCancel model =
    ( singleton { model | modal = Ui.Modal.close model.modal }, OutCancel )


updateModalMsg : Model -> Ui.Modal.Msg -> ( Return Msg Model, OutMsg )
updateModalMsg model modalMsg =
    ( singleton { model | modal = Ui.Modal.update modalMsg model.modal }, OutNone )


updateFormMsg : Model -> Form.Msg -> ( Return Msg Model, OutMsg )
updateFormMsg model formMsg =
    ( Form.update formMsg model.form
        |> Return.mapBoth FormMsg (\nf -> { model | form = nf })
    , OutNone
    )


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
