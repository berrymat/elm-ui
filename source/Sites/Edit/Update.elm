module Sites.Edit.Update exposing (..)

import Sites.Edit.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Helpers.Return as Return exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
import Ui.Modal
import Container.Out exposing (..)
import Sites.Site exposing (..)


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case msg of
        Save authToken ->
            updateValidate model authToken

        Cancel ->
            updateCancel model

        SaveResponse response ->
            updateSaveResponse model response

        ModalMsg modalMsg ->
            updateModalMsg model modalMsg

        FormMsg formMsg ->
            updateFormMsg model formMsg


updateValidate : Model -> AuthToken -> ReturnOut Msg OutMsg Model
updateValidate model token =
    let
        validate model =
            Form.update Form.Validate model.form
                |> wrap
                |> Return.mapBoth FormMsg (\f -> { model | form = f })

        saveIfValid model =
            if (Maybe.withDefault False model.form.valid) then
                updateSave model token
            else
                singleton model
    in
        model
            |> validate
            |> Return.andThen saveIfValid


updateSave : Model -> AuthToken -> ReturnOut Msg OutMsg Model
updateSave model authToken =
    let
        newSite =
            updateSite model.form model.site

        saveCmd =
            Helpers.Helpers.requester authToken
                "Sites"
                model.id
                model.method
                (encodeSite newSite)
                siteDecoder
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


updateFormMsg : Model -> Form.Msg -> ReturnOut Msg OutMsg Model
updateFormMsg model formMsg =
    Form.update formMsg model.form
        |> wrap
        |> Return.mapBoth FormMsg (\nf -> { model | form = nf })


updateSaveResponse : Model -> WebData Site -> ReturnOut Msg OutMsg Model
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        success value =
            out { newModel | modal = Ui.Modal.close model.modal }
                Cmd.none
                (OutUpdateSite model.method value)
    in
        RemoteData.map success response
            |> RemoteData.withDefault (return newModel cmd)
