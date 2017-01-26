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


customerForm : Customer -> Form.Model
customerForm customer =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "name", 0, "Name", (Maybe.withDefault "" customer.name), Nothing, [ Form.Validator requiredInput ] )
            , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" customer.address1), Nothing, [] )
            , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" customer.address2), Nothing, [] )
            , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" customer.address3), Nothing, [] )
            , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" customer.address4), Nothing, [] )
            , ( "postcode", 5, "Postcode", (Maybe.withDefault "" customer.postcode), Nothing, [] )
            , ( "contact", 6, "Contact", (Maybe.withDefault "" customer.contact), Nothing, [] )
            , ( "phone", 7, "Phone", (Maybe.withDefault "" customer.tel), Nothing, [] )
            , ( "email", 8, "Email", (Maybe.withDefault "" customer.email), Nothing, [ Form.Validator optionalValidEmail ] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }


updateCustomer : Form.Model -> Customer -> Customer
updateCustomer form customer =
    { customer
        | name = Just (Form.valueOfInput "name" "" form)
        , address1 = Just (Form.valueOfInput "address1" "" form)
        , address2 = Just (Form.valueOfInput "address2" "" form)
        , address3 = Just (Form.valueOfInput "address3" "" form)
        , address4 = Just (Form.valueOfInput "address4" "" form)
        , postcode = Just (Form.valueOfInput "postcode" "" form)
        , contact = Just (Form.valueOfInput "contact" "" form)
        , tel = Just (Form.valueOfInput "phone" "" form)
        , email = Just (Form.valueOfInput "email" "" form)
    }
