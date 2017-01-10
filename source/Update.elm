module Update exposing (..)

import Ui.App
import Models exposing (..)
import Helpers.Models exposing (..)
import Container.Models exposing (initialContainer)
import Login.Models
import Login.Update
import Container.Update
import Routing exposing (Route(..), parseLocation)
import Return exposing (..)
import HttpBuilder exposing (..)
import Helpers.Helpers exposing (..)
import Http
import RemoteData exposing (..)
import Navigation
import Ui.NotificationCenter
import View exposing (..)
import Json.Decode as Decode exposing (field, at)


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
            tryUpdateContainer model RootType id CustomerType

        ClientRoute id ->
            tryUpdateContainer model CustomerType id ClientType

        StaffRoute id ->
            tryUpdateContainer model CustomerType id StaffType

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

        HandleNotification value ->
            updateHandleNotification model value

        NotificationMsg subMsg ->
            updateNotification model subMsg


updateContainer : Model -> Container.Models.Msg -> Return Msg Model
updateContainer model subMsg =
    Container.Update.update subMsg model.container
        |> Return.mapBoth ContainerMsg (\new -> { model | container = new })


tryUpdateContainer : Model -> NodeType -> NodeId -> NodeType -> Return Msg Model
tryUpdateContainer model parentType id childType =
    case model.login.authResult of
        NotAsked ->
            let
                ( newLogin, loginMsg ) =
                    Login.Update.update Login.Models.LoadToken model.login
            in
                ( { model | login = newLogin }, Cmd.map LoginMsg loginMsg )

        Loading ->
            singleton model

        Failure error ->
            singleton model

        Success result ->
            updateContainer model (Container.Models.LoadContainer parentType id childType)


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


updateHandleNotification : Model -> Decode.Value -> Return Msg Model
updateHandleNotification model value =
    let
        result =
            Decode.decodeValue notificationDecoder value
    in
        case result of
            Ok notification ->
                Ui.NotificationCenter.notify
                    (viewNotification notification)
                    model.notificationCenter
                    |> Return.mapBoth NotificationMsg (\new -> { model | notificationCenter = new })

            Err error ->
                singleton model


updateNotification : Model -> Ui.NotificationCenter.Msg -> Return Msg Model
updateNotification model subMsg =
    Ui.NotificationCenter.update subMsg model.notificationCenter
        |> Return.mapBoth NotificationMsg (\new -> { model | notificationCenter = new })
