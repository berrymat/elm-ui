module Reset.Update exposing (..)

import Reset.Models exposing (..)
import Return exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import HttpBuilder exposing (..)
import Http
import RemoteData exposing (..)
import Login.Update exposing (authenticateDecoder)
import Users.Models exposing (encodeChangePassword, ChangePassword)
import Components.Form as Form
import Ui.Modal
import Navigation


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ResetFormMsg formMsg ->
            Form.update formMsg model.resetForm
                |> Return.map (\nf -> { model | resetForm = nf })
                |> mapCmd ResetFormMsg

        ResetModalMsg modalMsg ->
            ( { model | resetModal = Ui.Modal.update modalMsg model.resetModal }
            , Cmd.none
            )

        LoadReset ->
            singleton model

        AuthenticateResponse webdata ->
            updateAuthenticateResponse model webdata

        LoadToken resetToken ->
            updateLoadToken model resetToken

        SaveResetModal authToken ->
            updateSaveResetModal model authToken

        PasswordResetResponse webdata ->
            updatePasswordResetResponse model webdata


fetchAuthResult : String -> Cmd Msg
fetchAuthResult resetToken =
    HttpBuilder.get (apiUrl ++ "Reset/" ++ resetToken)
        |> withExpect (Http.expectJson authenticateDecoder)
        |> withCredentials
        |> send (AuthenticateResponse << RemoteData.fromResult)


updateLoadToken : Model -> String -> Return Msg Model
updateLoadToken model resetToken =
    case model.authResult of
        NotAsked ->
            singleton model
                |> Return.map (\model -> { model | resetToken = resetToken })
                |> command (fetchAuthResult resetToken)

        Loading ->
            singleton model

        Failure error ->
            singleton model

        Success result ->
            singleton result
                |> Return.map (\new -> { model | authResult = Success new })


updateAuthenticateResponse : Model -> WebData AuthResult -> Return Msg Model
updateAuthenticateResponse model authResult =
    RemoteData.map (updateAuthenticateResponseSuccess model) authResult
        |> RemoteData.withDefault (singleton model)


updateAuthenticateResponseSuccess : Model -> AuthResult -> Return Msg Model
updateAuthenticateResponseSuccess model authResult =
    let
        newResetModal =
            Ui.Modal.open model.resetModal
    in
        ( { model | resetModal = newResetModal, authResult = Success authResult }, Cmd.none )


updateSaveResetModal : Model -> AuthToken -> Return Msg Model
updateSaveResetModal model token =
    let
        changePassword =
            { id = ""
            , password = Form.valueOfInput "New Password" "" model.resetForm
            , confirmPassword = Form.valueOfInput "Confirm Password" "" model.resetForm
            }
    in
        ( model
        , putter token "Reset" model.resetToken (encodeChangePassword changePassword) authenticateDecoder (PasswordResetResponse << RemoteData.fromResult)
        )


updatePasswordResetResponse : Model -> WebData AuthResult -> Return Msg Model
updatePasswordResetResponse model authResult =
    RemoteData.map (updatePasswordResetResponseSuccess model) authResult
        |> RemoteData.withDefault (singleton model)


updatePasswordResetResponseSuccess : Model -> AuthResult -> Return Msg Model
updatePasswordResetResponseSuccess model authResult =
    singleton authResult
        |> Return.map (\new -> { model | authResult = Success new })
        |> Return.command (Navigation.newUrl ("#Login"))
