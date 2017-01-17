module Customers.Delete.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Customers.Customer exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Customer)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , customer : Customer
    , modal : Ui.Modal.Model
    , response : WebData Customer
    }


init : Customer -> Model
init customer =
    { id = customer.id
    , customer = customer
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
