module Roots.Actions.Update exposing (..)

import Roots.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Roots.Actions.Out exposing (..)
import Roots.Root exposing (..)
import Roots.Edit.Models as EditModels
import Roots.Delete.Models as DeleteModels
import Roots.Edit.Update as Edit
import Roots.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType root ) ->
                ( updateOpen model modalType root, OutNone )

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


updateOpen : Model -> ModalType -> Root -> Return Msg Model
updateOpen model modalType root =
    case modalType of
        NewRoot ->
            singleton
                (EditModel (EditModels.init root Post))

        EditRoot ->
            singleton
                (EditModel (EditModels.init root Put))

        DeleteRoot ->
            singleton (DeleteModel (DeleteModels.init root))
