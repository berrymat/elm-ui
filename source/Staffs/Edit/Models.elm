module Staffs.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Staffs.Staff exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Staff)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , staff : Staff
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Staff
    }


init : Staff -> HttpMethod -> Model
init staff method =
    { id = staff.id
    , staff = staff
    , method = method
    , form = staffForm staff
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


staffForm : Staff -> Form.Model
staffForm staff =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "name", 0, "Name", (Maybe.withDefault "" staff.name), Nothing, [ Form.Validator requiredInput ] )
            , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" staff.address1), Nothing, [] )
            , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" staff.address2), Nothing, [] )
            , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" staff.address3), Nothing, [] )
            , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" staff.address4), Nothing, [] )
            , ( "postcode", 5, "Postcode", (Maybe.withDefault "" staff.postcode), Nothing, [] )
            , ( "phone", 7, "Phone", (Maybe.withDefault "" staff.tel), Nothing, [] )
            , ( "mobile", 6, "Mobile", (Maybe.withDefault "" staff.mob), Nothing, [] )
            , ( "email", 8, "Email", (Maybe.withDefault "" staff.email), Nothing, [ Form.Validator optionalValidEmail ] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }


updateStaff : Form.Model -> Staff -> Staff
updateStaff form staff =
    { staff
        | name = Just (Form.valueOfInput "name" (Maybe.withDefault "" staff.name) form)
        , address1 = Just (Form.valueOfInput "address1" (Maybe.withDefault "" staff.address1) form)
        , address2 = Just (Form.valueOfInput "address2" (Maybe.withDefault "" staff.address2) form)
        , address3 = Just (Form.valueOfInput "address3" (Maybe.withDefault "" staff.address3) form)
        , address4 = Just (Form.valueOfInput "address4" (Maybe.withDefault "" staff.address4) form)
        , postcode = Just (Form.valueOfInput "postcode" (Maybe.withDefault "" staff.postcode) form)
        , tel = Just (Form.valueOfInput "phone" (Maybe.withDefault "" staff.tel) form)
        , mob = Just (Form.valueOfInput "mobile" (Maybe.withDefault "" staff.mob) form)
        , email = Just (Form.valueOfInput "email" (Maybe.withDefault "" staff.email) form)
    }
