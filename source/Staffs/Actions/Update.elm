module Staffs.Actions.Update exposing (..)

import Staffs.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Staffs.Actions.Out exposing (..)
import Staffs.Staff exposing (..)
import Staffs.Edit.Models as EditModels
import Staffs.Delete.Models as DeleteModels
import Staffs.Edit.Update as Edit
import Staffs.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType staff ) ->
                ( updateOpen model modalType staff, OutNone )

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


updateOpen : Model -> ModalType -> Staff -> Return Msg Model
updateOpen model modalType staff =
    case modalType of
        NewStaff ->
            singleton
                (EditModel (EditModels.init staff Post))

        EditStaff ->
            singleton
                (EditModel (EditModels.init staff Put))

        DeleteStaff ->
            singleton (DeleteModel (DeleteModels.init staff))
