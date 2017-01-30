module Issues.Actions.Update exposing (..)

import Issues.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Issues.Issue exposing (..)
import Issues.Edit.Models as EditModels
import Issues.Edit.Update as Edit


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType sites issue ) ->
            updateOpen model modalType sites issue

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> List IssueSite -> Issue -> ReturnOut Msg OutMsg Model
updateOpen model modalType sites issue =
    case modalType of
        NewIssue ->
            singleton
                (EditModel (EditModels.init sites issue Post))

        EditIssue ->
            singleton
                (EditModel (EditModels.init sites issue Put))
