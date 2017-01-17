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


accessName : String
accessName =
    "Access?"


staffForm : Staff -> Form.Model
staffForm staff =
    Form.init
        { checkboxes =
            [ ( accessName, 11, staff.access, [] )
            ]
        , inputs =
            [ ( "Name", 1, "Name", staff.name, Nothing, [ Form.Validator requiredInput ] )
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


updateStaff : Form.Model -> Staff -> Staff
updateStaff form staff =
    { staff
        | name = Form.valueOfInput "Name" staff.name form
        , access = Form.valueOfCheckbox accessName staff.access form
    }
