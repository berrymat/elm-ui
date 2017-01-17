module Customers.Actions.Models exposing (..)

import Customers.Edit.Models as Edit
import Customers.Delete.Models as Delete
import Customers.Customer exposing (..)


type ModalType
    = NewCustomer
    | EditCustomer
    | DeleteCustomer


type Model
    = EditModel Edit.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType Customer
    | EditMsg Edit.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
