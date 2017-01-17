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
    | SaveResponse (WebData Client)
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
    , form = clientForm client
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


accessName : String
accessName =
    "Access?"


clientForm : Client -> Form.Model
clientForm client =
    Form.init
        { checkboxes =
            [ ( accessName, 11, client.access, [] )
            ]
        , inputs =
            [ ( "Name", 1, "Name", client.name, Nothing, [ Form.Validator requiredInput ] )
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


updateClient : Form.Model -> Client -> Client
updateClient form client =
    { client
        | name = Form.valueOfInput "Name" client.name form
        , access = Form.valueOfCheckbox accessName client.access form
    }
