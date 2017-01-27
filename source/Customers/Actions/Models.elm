module Customers.Actions.Models exposing (..)

import Customers.Edit.Models
import Customers.Delete.Models
import Clients.Edit.Models
import Staffs.Edit.Models
import Customers.Customer exposing (..)


type ModalType
    = EditCustomer
    | DeleteCustomer
    | NewClient
    | NewStaff


type Model
    = EditCustomerModel Customers.Edit.Models.Model
    | DeleteCustomerModel Customers.Delete.Models.Model
    | EditClientModel Clients.Edit.Models.Model
    | EditStaffModel Staffs.Edit.Models.Model
    | NoModel


type Msg
    = Open ModalType Customer
    | EditCustomerMsg Customers.Edit.Models.Msg
    | DeleteCustomerMsg Customers.Delete.Models.Msg
    | EditClientMsg Clients.Edit.Models.Msg
    | EditStaffMsg Staffs.Edit.Models.Msg


init : Model
init =
    NoModel
