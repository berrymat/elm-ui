module Issues.Actions.Update exposing (..)

import Issues.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Issues.Actions.Out exposing (..)
import Issues.Issue exposing (..)
import Issues.Edit.Models as EditModels
import Issues.Edit.Update as Edit


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType sites issue ) ->
                ( updateOpen model modalType sites issue, OutNone )

            ( EditModel model_, EditMsg msg_ ) ->
                Edit.update msg_ model_
                    |> mapBothEx EditMsg EditModel

            x ->
                let
                    _ =
                        Debug.log "Stray found" x
                in
                    ( singleton model, OutNone )


updateOpen : Model -> ModalType -> List IssueSite -> Issue -> Return Msg Model
updateOpen model modalType sites issue =
    case modalType of
        NewIssue ->
            singleton
                (EditModel (EditModels.init sites issue Post))

        EditIssue ->
            singleton
                (EditModel (EditModels.init sites issue Put))
