module Users.Edit.Update exposing (..)

import Users.Edit.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
import Ui.Modal
import Users.Actions.Out exposing (..)
import Users.User exposing (..)


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
        newUser =
            updateUser model.form model.user

        saveCmd =
            Helpers.Helpers.requester authToken
                "Users"
                model.id
                model.method
                (encodeUser newUser)
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
