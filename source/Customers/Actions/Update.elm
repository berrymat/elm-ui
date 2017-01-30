module Customers.Actions.Update exposing (..)

import Customers.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
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


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType customer ) ->
            updateOpen model modalType customer

        ( EditCustomerModel model_, EditCustomerMsg msg_ ) ->
            Customers.Edit.Update.update msg_ model_
                |> mapBoth EditCustomerMsg EditCustomerModel

        ( DeleteCustomerModel model_, DeleteCustomerMsg msg_ ) ->
            Customers.Delete.Update.update msg_ model_
                |> mapBoth DeleteCustomerMsg DeleteCustomerModel

        ( EditClientModel model_, EditClientMsg msg_ ) ->
            Clients.Edit.Update.update msg_ model_
                |> mapBoth EditClientMsg EditClientModel

        ( EditStaffModel model_, EditStaffMsg msg_ ) ->
            Staffs.Edit.Update.update msg_ model_
                |> mapBoth EditStaffMsg EditStaffModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> Customer -> ReturnOut Msg OutMsg Model
updateOpen model modalType customer =
    case modalType of
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
