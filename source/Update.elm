module Update exposing (..)

import Ui.App
import Messages exposing (Msg(..))
import Models exposing (Model)
import Helpers.Models exposing (..)
import Container.Messages
import Container.Update
import Routing exposing (Route(..), parseLocation)


fetchData : Model -> ( Model, Cmd Msg )
fetchData model =
    case model.route of
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
