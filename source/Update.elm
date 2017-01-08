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


updateContainer : Model -> Container.Messages.Msg -> Return Msg Model
updateContainer model subMsg =
    Container.Update.update subMsg model.container
        |> Return.mapBoth ContainerMsg (\new -> { model | container = new })


fetchData : Model -> Return Msg Model
fetchData model =
    case model.route of
        LoginRoute ->
            let
                ( newLogin, loginMsg ) =
                    Login.Update.update Login.Models.OpenLoginModal
                        model.login
            in
                ( { model | login = newLogin }, Cmd.map LoginMsg loginMsg )

        HomeRoute ->
            let
                ( newLogin, loginMsg ) =
                    Login.Update.update Login.Models.GotoHome
                        model.login
            in
                ( { model | login = newLogin }, Cmd.map LoginMsg loginMsg )

        CustomerRoute id ->
            updateContainer model (Container.Messages.LoadContainer RootType id CustomerType)

        ClientRoute id ->
            updateContainer model (Container.Messages.LoadContainer CustomerType id ClientType)

        StaffRoute id ->
            updateContainer model (Container.Messages.LoadContainer CustomerType id StaffType)

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
            updateContainer model subMsg

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
