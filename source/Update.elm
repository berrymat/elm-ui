module Update exposing (..)

import Ui.App
import Messages exposing (Msg(..))
import Models exposing (Model)
import Helpers.Models exposing (..)
import Container.Models exposing (initialContainer)
import Login.Models
import Login.Update
import Container.Messages
import Container.Update
import Routing exposing (Route(..), parseLocation)
import Return exposing (..)
import HttpBuilder exposing (..)
import Helpers.Helpers exposing (..)
import Http
import Json.Decode as Decode
import RemoteData exposing (..)
import Navigation


fetchData : Model -> Return Msg Model
fetchData model =
    case model.route of
        LoginRoute ->
            let
                ( newLogin, loginMsg ) =
                    Login.Update.update Login.Models.LoadLogin
                        model.login
            in
                ( { model | login = newLogin }, Cmd.map LoginMsg loginMsg )

        HomeRoute ->
            let
                ( newContainer, containerMsg ) =
                    Container.Update.update (Container.Messages.LoadContainer RootType "" RootType)
                        model.container
            in
                ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

        CustomerRoute id ->
            let
                ( newContainer, containerMsg ) =
                    Container.Update.update (Container.Messages.LoadContainer RootType id CustomerType)
                        model.container
            in
                ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

        ClientRoute id ->
            let
                ( newContainer, containerMsg ) =
                    Container.Update.update (Container.Messages.LoadContainer CustomerType id ClientType)
                        model.container
            in
                ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

        StaffRoute id ->
            let
                ( newContainer, containerMsg ) =
                    Container.Update.update (Container.Messages.LoadContainer CustomerType id StaffType)
                        model.container
            in
                ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

        NotFoundRoute ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        App act ->
            let
                ( app, effect ) =
                    Ui.App.update act model.app
            in
                ( { model | app = app }, Cmd.map App effect )

        Logout ->
            updateLogout model

        LogoutResponse response ->
            updateLogoutResponse model response

        LoginMsg subMsg ->
            let
                ( updatedLogin, cmd ) =
                    Login.Update.update
                        subMsg
                        model.login
            in
                ( { model | login = updatedLogin }, Cmd.map LoginMsg cmd )

        ContainerMsg subMsg ->
            let
                ( updatedContainer, cmd ) =
                    Container.Update.update
                        subMsg
                        model.container
            in
                ( { model | container = updatedContainer }, Cmd.map ContainerMsg cmd )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                newModel =
                    { model | route = newRoute }
            in
                fetchData newModel


updateLogout : Model -> Return Msg Model
updateLogout model =
    let
        cmd =
            HttpBuilder.post (apiUrl ++ "Logout")
                |> withExpect (Http.expectJson Decode.bool)
                |> withCredentials
                |> send (LogoutResponse << RemoteData.fromResult)
    in
        ( model, cmd )


updateLogoutResponse : Model -> WebData Bool -> Return Msg Model
updateLogoutResponse model response =
    let
        ( newModel, cmd ) =
            RemoteData.map
                (\signedOut ->
                    if signedOut then
                        ( { model | container = initialContainer }
                        , (Navigation.newUrl "#Login")
                        )
                    else
                        ( model, Cmd.none )
                )
                response
                |> RemoteData.withDefault ( model, Cmd.none )
    in
        ( { newModel | signedIn = RemoteData.map not response }, cmd )
