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


accessName : String
accessName =
    "Access?"


siteForm : Site -> Form.Model
siteForm site =
    Form.init
        { checkboxes =
            [ ( accessName, 11, site.access, [] )
            ]
        , inputs =
            [ ( "Name", 1, "Name", site.name, Nothing, [ Form.Validator requiredInput ] )
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


updateSite : Form.Model -> Site -> Site
updateSite form site =
    { site
        | name = Form.valueOfInput "Name" site.name form
        , access = Form.valueOfCheckbox accessName site.access form
    }
