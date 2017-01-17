module Customers.Actions.Update exposing (..)

import Customers.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Customers.Actions.Out exposing (..)
import Customers.Customer exposing (..)
import Customers.Edit.Models as EditModels
import Customers.Delete.Models as DeleteModels
import Customers.Edit.Update as Edit
import Customers.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType customer ) ->
                ( updateOpen model modalType customer, OutNone )

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


updateOpen : Model -> ModalType -> Customer -> Return Msg Model
updateOpen model modalType customer =
    case modalType of
        NewCustomer ->
            singleton
                (EditModel (EditModels.init customer Post))

        EditCustomer ->
            singleton
                (EditModel (EditModels.init customer Put))

        DeleteCustomer ->
            singleton (DeleteModel (DeleteModels.init customer))
