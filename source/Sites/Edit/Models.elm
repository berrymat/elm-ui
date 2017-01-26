module Sites.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Sites.Site exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Site)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , site : Site
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Site
    }


init : Site -> HttpMethod -> Model
init site method =
    { id = site.id
    , site = site
    , method = method
    , form = siteForm site
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


siteForm : Site -> Form.Model
siteForm site =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "name", 0, "Name", (Maybe.withDefault "" site.name), Nothing, [ Form.Validator requiredInput ] )
            , ( "address1", 1, "Address Line 1", (Maybe.withDefault "" site.address1), Nothing, [] )
            , ( "address2", 2, "Address Line 2", (Maybe.withDefault "" site.address2), Nothing, [] )
            , ( "address3", 3, "Address Line 3", (Maybe.withDefault "" site.address3), Nothing, [] )
            , ( "address4", 4, "Address Line 4", (Maybe.withDefault "" site.address4), Nothing, [] )
            , ( "postcode", 5, "Postcode", (Maybe.withDefault "" site.postcode), Nothing, [] )
            , ( "contact", 6, "Contact", (Maybe.withDefault "" site.contact), Nothing, [] )
            , ( "phone", 7, "Phone", (Maybe.withDefault "" site.tel), Nothing, [] )
            , ( "email", 8, "Email", (Maybe.withDefault "" site.email), Nothing, [ Form.Validator optionalValidEmail ] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }


updateSite : Form.Model -> Site -> Site
updateSite form site =
    { site
        | name = Just (Form.valueOfInput "Name" (Maybe.withDefault "" site.name) form)
        , address1 = Just (Form.valueOfInput "address1" (Maybe.withDefault "" site.address1) form)
        , address2 = Just (Form.valueOfInput "address2" (Maybe.withDefault "" site.address2) form)
        , address3 = Just (Form.valueOfInput "address3" (Maybe.withDefault "" site.address3) form)
        , address4 = Just (Form.valueOfInput "address4" (Maybe.withDefault "" site.address4) form)
        , postcode = Just (Form.valueOfInput "postcode" (Maybe.withDefault "" site.postcode) form)
        , contact = Just (Form.valueOfInput "contact" (Maybe.withDefault "" site.contact) form)
        , tel = Just (Form.valueOfInput "phone" (Maybe.withDefault "" site.tel) form)
        , email = Just (Form.valueOfInput "email" (Maybe.withDefault "" site.email) form)
    }
