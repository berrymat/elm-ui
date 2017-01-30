module Roots.Actions.Update exposing (..)

import Roots.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Roots.Root exposing (..)
import Roots.Edit.Models as EditModels
import Roots.Delete.Models as DeleteModels
import Roots.Edit.Update as Edit
import Roots.Delete.Update as Delete


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType root ) ->
            updateOpen model modalType root

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        ( DeleteModel model_, DeleteMsg msg_ ) ->
            Delete.update msg_ model_
                |> mapBoth DeleteMsg DeleteModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> Root -> ReturnOut Msg OutMsg Model
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
