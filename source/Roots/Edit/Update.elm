module Roots.Edit.Update exposing (..)

import Roots.Edit.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
import Ui.Modal
import Container.Out exposing (..)
import Roots.Root exposing (..)


update : Msg -> Model -> ( Return Msg Model, OutMsg )
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


updateValidate : Model -> AuthToken -> ( Return Msg Model, OutMsg )
updateValidate model token =
    let
        ( validModel, validEffect ) =
            Form.update Form.Validate model.form
                |> Return.mapBoth FormMsg (\f -> { model | form = f })

        ( ( saveModel, saveEffect ), saveOut ) =
            case validModel.form.valid of
                Just valid ->
                    if valid then
                        updateSave validModel token
                    else
                        ( singleton validModel, OutNone )

                Nothing ->
                    ( singleton validModel, OutNone )
    in
        ( ( saveModel
          , Cmd.batch [ validEffect, saveEffect ]
          )
        , saveOut
        )


updateSave : Model -> AuthToken -> ( Return Msg Model, OutMsg )
updateSave model authToken =
    let
        newRoot =
            updateRoot model.form model.root

        saveCmd =
            Helpers.Helpers.requester authToken
                "Roots"
                model.id
                model.method
                (encodeRoot newRoot)
                rootDecoder
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


updateSaveResponse : Model -> WebData Root -> ( Return Msg Model, OutMsg )
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        updateSaveResponseSuccess root =
            ( singleton { newModel | modal = Ui.Modal.close model.modal }
            , OutUpdateRoot model.method root
            )
    in
        RemoteData.map updateSaveResponseSuccess response
            |> RemoteData.withDefault ( return newModel cmd, OutNone )
