module Sites.Actions.Update exposing (..)

import Sites.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Sites.Site exposing (..)
import Sites.Edit.Models as EditModels
import Sites.Delete.Models as DeleteModels
import Sites.Edit.Update as Edit
import Sites.Delete.Update as Delete


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType site ) ->
            updateOpen model modalType site

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        ( DeleteModel model_, DeleteMsg msg_ ) ->
            Delete.update msg_ model_
                |> mapBoth DeleteMsg DeleteModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> Site -> ReturnOut Msg OutMsg Model
updateOpen model modalType site =
    case modalType of
        NewSite ->
            singleton
                (EditModel (EditModels.init site Post))

        EditSite ->
            singleton
                (EditModel (EditModels.init site Put))

        DeleteSite ->
            singleton (DeleteModel (DeleteModels.init site))
