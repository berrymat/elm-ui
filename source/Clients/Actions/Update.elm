module Clients.Actions.Update exposing (..)

import Clients.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Clients.Client exposing (..)
import Clients.Edit.Models as EditModels
import Clients.Delete.Models as DeleteModels
import Clients.Edit.Update as Edit
import Clients.Delete.Update as Delete


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType client ) ->
            updateOpen model modalType client

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        ( DeleteModel model_, DeleteMsg msg_ ) ->
            Delete.update msg_ model_
                |> mapBoth DeleteMsg DeleteModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> Client -> ReturnOut Msg OutMsg Model
updateOpen model modalType client =
    case modalType of
        NewClient ->
            singleton
                (EditModel (EditModels.init client Post))

        EditClient ->
            singleton
                (EditModel (EditModels.init client Put))

        DeleteClient ->
            singleton (DeleteModel (DeleteModels.init client))
