module Update exposing (..)

import Ui.App
import Messages exposing (Msg(..))
import Models exposing (Model)
import Helpers.Models exposing (..)
import Container.Messages
import Container.Update
import Routing exposing (Route(..), parseLocation)
import Container.Commands
import Tree.Models exposing (convertNodeType)


fetchData : Model -> ( Model, Cmd Msg )
fetchData model =
    case model.route of
        ContainerRoot ->
            let
                ( newContainer, containerMsg ) =
                    Container.Update.update (Container.Messages.LoadContainer RootType "")
                        model.container
            in
                ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

        ContainerRoute type_ id ->
            let
                maybeNodeType =
                    Debug.log "convertNodeType" (convertNodeType type_)
            in
                case maybeNodeType of
                    Just nodeType ->
                        let
                            ( newContainer, containerMsg ) =
                                Container.Update.update (Container.Messages.LoadContainer nodeType id)
                                    model.container
                        in
                            ( { model | container = newContainer }, Cmd.map ContainerMsg containerMsg )

                    Nothing ->
                        ( model, Cmd.none )

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
