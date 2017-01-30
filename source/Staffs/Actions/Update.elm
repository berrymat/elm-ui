module Staffs.Actions.Update exposing (..)

import Staffs.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Staffs.Staff exposing (..)
import Staffs.Edit.Models as EditModels
import Staffs.Delete.Models as DeleteModels
import Staffs.Edit.Update as Edit
import Staffs.Delete.Update as Delete


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType staff ) ->
            updateOpen model modalType staff

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        ( DeleteModel model_, DeleteMsg msg_ ) ->
            Delete.update msg_ model_
                |> mapBoth DeleteMsg DeleteModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> Staff -> ReturnOut Msg OutMsg Model
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
