module Sites.Actions.Update exposing (..)

import Sites.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Sites.Site exposing (..)
import Sites.Edit.Models as EditModels
import Sites.Delete.Models as DeleteModels
import Sites.Edit.Update as Edit
import Sites.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType site ) ->
                ( updateOpen model modalType site, OutNone )

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


updateOpen : Model -> ModalType -> Site -> Return Msg Model
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
