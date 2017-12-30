module Clients.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Clients.Client exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse NodeId (WebData Client)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , client : Client
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Client
    }


init : Client -> HttpMethod -> Model
init client method =
    { id = client.id
    , client = client
    , method = method
    , form = clientForm client method
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


clientForm : Client -> HttpMethod -> Form.Model
clientForm client method =
    let
        allinputs =
            [ ( "no", 0, "No", (Maybe.withDefault "" client.no), Nothing, [ Form.Validator requiredInput ] )
            , ( "name", 1, "Name", (Maybe.withDefault "" client.name), Nothing, [ Form.Validator requiredInput ] )
            , ( "address1", 2, "Address Line 1", (Maybe.withDefault "" client.address1), Nothing, [] )
            , ( "address2", 3, "Address Line 2", (Maybe.withDefault "" client.address2), Nothing, [] )
            , ( "address3", 4, "Address Line 3", (Maybe.withDefault "" client.address3), Nothing, [] )
            , ( "address4", 5, "Address Line 4", (Maybe.withDefault "" client.address4), Nothing, [] )
            , ( "postcode", 6, "Postcode", (Maybe.withDefault "" client.postcode), Nothing, [] )
            , ( "contact", 7, "Contact", (Maybe.withDefault "" client.contact), Nothing, [] )
            , ( "phone", 8, "Phone", (Maybe.withDefault "" client.tel), Nothing, [] )
            , ( "email", 9, "Email", (Maybe.withDefault "" client.email), Nothing, [ Form.Validator optionalValidEmail ] )
            ]

        inputs =
            if method == Post then
                allinputs
            else
                allinputs |> List.tail |> Maybe.withDefault []
    in
        Form.init
            { checkboxes = []
            , inputs = inputs
            , fileInputs = []
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }


updateClient : Form.Model -> Client -> Client
updateClient form client =
    { client
        | no = Just (Form.valueOfInput "no" (Maybe.withDefault "" client.no) form)
        , name = Just (Form.valueOfInput "name" (Maybe.withDefault "" client.name) form)
        , address1 = Just (Form.valueOfInput "address1" (Maybe.withDefault "" client.address1) form)
        , address2 = Just (Form.valueOfInput "address2" (Maybe.withDefault "" client.address2) form)
        , address3 = Just (Form.valueOfInput "address3" (Maybe.withDefault "" client.address3) form)
        , address4 = Just (Form.valueOfInput "address4" (Maybe.withDefault "" client.address4) form)
        , postcode = Just (Form.valueOfInput "postcode" (Maybe.withDefault "" client.postcode) form)
        , contact = Just (Form.valueOfInput "contact" (Maybe.withDefault "" client.contact) form)
        , tel = Just (Form.valueOfInput "phone" (Maybe.withDefault "" client.tel) form)
        , email = Just (Form.valueOfInput "email" (Maybe.withDefault "" client.email) form)
    }
