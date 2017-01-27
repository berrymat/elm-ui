module Clients.Actions.Update exposing (..)

import Clients.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Clients.Client exposing (..)
import Clients.Edit.Models as EditModels
import Clients.Delete.Models as DeleteModels
import Clients.Edit.Update as Edit
import Clients.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType client ) ->
                ( updateOpen model modalType client, OutNone )

            ( EditModel model_, EditMsg msg_ ) ->
                Edit.update msg_ model_
                    |> mapBothEx EditMsg EditModel

            ( DeleteModel model_, DeleteMsg msg_ ) ->
                Delete.update msg_ model_
                    |> mapBothEx DeleteMsg DeleteModel

            x ->
                let
                    _ =
                        Debug.log "Stray found" x
                in
                    ( singleton model, OutNone )


updateOpen : Model -> ModalType -> Client -> Return Msg Model
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
