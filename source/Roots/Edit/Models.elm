module Roots.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Roots.Root exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Root)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , root : Root
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Root
    }


init : Root -> HttpMethod -> Model
init root method =
    { id = root.id
    , root = root
    , method = method
    , form = rootForm root
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


rootForm : Root -> Form.Model
rootForm root =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "name", 0, "Name", (Maybe.withDefault "" root.name), Nothing, [ Form.Validator requiredInput ] )
            , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" root.address1), Nothing, [] )
            , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" root.address2), Nothing, [] )
            , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" root.address3), Nothing, [] )
            , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" root.address4), Nothing, [] )
            , ( "postcode", 5, "Postcode", (Maybe.withDefault "" root.postcode), Nothing, [] )
            , ( "contact", 6, "Contact", (Maybe.withDefault "" root.contact), Nothing, [] )
            , ( "phone", 7, "Phone", (Maybe.withDefault "" root.tel), Nothing, [] )
            , ( "email", 8, "Email", (Maybe.withDefault "" root.email), Nothing, [ Form.Validator optionalValidEmail ] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }


updateRoot : Form.Model -> Root -> Root
updateRoot form root =
    { root
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
