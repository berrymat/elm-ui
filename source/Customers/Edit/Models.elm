module Customers.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Customers.Customer exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Customer)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , customer : Customer
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Customer
    }


init : Customer -> HttpMethod -> Model
init customer method =
    { id = customer.id
    , customer = customer
    , method = method
    , form = customerForm customer
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


accessName : String
accessName =
    "Access?"


customerForm : Customer -> Form.Model
customerForm customer =
    Form.init
        { checkboxes =
            [ ( accessName, 11, customer.access, [] )
            ]
        , inputs =
            [ ( "Name", 1, "Name", customer.name, Nothing, [ Form.Validator requiredInput ] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles =
            [ ( "title", 10, "Title" )
            ]
        }


updateCustomer : Form.Model -> Customer -> Customer
updateCustomer form customer =
    { customer
        | name = Form.valueOfInput "Name" customer.name form
        , access = Form.valueOfCheckbox accessName customer.access form
    }
