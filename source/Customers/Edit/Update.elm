module Customers.Edit.Update exposing (..)

import Customers.Edit.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
import Ui.Modal
import Container.Out exposing (..)
import Customers.Customer exposing (..)


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
        newCustomer =
            updateCustomer model.form model.customer

        saveCmd =
            Helpers.Helpers.requester authToken
                "Customers"
                model.id
                model.method
                (encodeCustomer newCustomer)
                customerDecoder
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


updateSaveResponse : Model -> WebData Customer -> ( Return Msg Model, OutMsg )
updateSaveResponse model response =
    let
        newModel =
            { model | response = response }

        cmd =
            Helpers.Helpers.errorCmd response

        updateSaveResponseSuccess customer =
            ( singleton { newModel | modal = Ui.Modal.close model.modal }
            , OutUpdateCustomer model.method customer
            )
    in
        RemoteData.map updateSaveResponseSuccess response
            |> RemoteData.withDefault ( return newModel cmd, OutNone )
