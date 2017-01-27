module Customers.Actions.Update exposing (..)

import Customers.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
import Customers.Customer exposing (..)
import Customers.Edit.Models
import Customers.Delete.Models
import Customers.Edit.Update
import Customers.Delete.Update
import Clients.Models
import Staffs.Models
import Clients.Edit.Models
import Staffs.Edit.Models
import Clients.Edit.Update
import Staffs.Edit.Update


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType customer ) ->
                ( updateOpen model modalType customer, OutNone )

            ( EditCustomerModel model_, EditCustomerMsg msg_ ) ->
                Customers.Edit.Update.update msg_ model_
                    |> mapBothEx EditCustomerMsg EditCustomerModel

            ( DeleteCustomerModel model_, DeleteCustomerMsg msg_ ) ->
                Customers.Delete.Update.update msg_ model_
                    |> mapBothEx DeleteCustomerMsg DeleteCustomerModel

            ( EditClientModel model_, EditClientMsg msg_ ) ->
                Clients.Edit.Update.update msg_ model_
                    |> mapBothEx EditClientMsg EditClientModel

            ( EditStaffModel model_, EditStaffMsg msg_ ) ->
                Staffs.Edit.Update.update msg_ model_
                    |> mapBothEx EditStaffMsg EditStaffModel

            x ->
                let
                    _ =
                        Debug.log "Stray found" x
                in
                    ( singleton model, OutNone )


updateOpen : Model -> ModalType -> Customer -> Return Msg Model
updateOpen model modalType customer =
    case modalType of
        {-
           NewCustomer ->
               singleton
                   (EditModel (EditModels.init customer Post))
        -}
        EditCustomer ->
            singleton
                (EditCustomerModel (Customers.Edit.Models.init customer Put))

        DeleteCustomer ->
            singleton (DeleteCustomerModel (Customers.Delete.Models.init customer))

        NewClient ->
            let
                client =
                    Clients.Models.initClient customer.id
            in
                singleton
                    (EditClientModel (Clients.Edit.Models.init client Post))

        NewStaff ->
            let
                staff =
                    Staffs.Models.initStaff customer.id
            in
                singleton
                    (EditStaffModel (Staffs.Edit.Models.init staff Post))
