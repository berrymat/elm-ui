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


accessName : String
accessName =
    "Access?"


rootForm : Root -> Form.Model
rootForm root =
    Form.init
        { checkboxes =
            [ ( accessName, 11, root.access, [] )
            ]
        , inputs =
            [ ( "Name", 1, "Name", root.name, Nothing, [ Form.Validator requiredInput ] )
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


updateRoot : Form.Model -> Root -> Root
updateRoot form root =
    { root
        | name = Form.valueOfInput "Name" root.name form
        , access = Form.valueOfCheckbox accessName root.access form
    }
