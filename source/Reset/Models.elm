module Reset.Models exposing (..)

import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)
import Helpers.Models exposing (..)


type Msg
    = LoadReset
    | AuthenticateResponse (WebData AuthResult)
    | LoadToken String
    | ResetFormMsg Form.Msg
    | ResetModalMsg Ui.Modal.Msg
    | SaveResetModal AuthToken
    | PasswordResetResponse (WebData AuthResult)


type alias Model =
    { resetToken : String
    , authResult : WebData AuthResult
    , resetModal : Ui.Modal.Model
    , resetForm : Form.Model
    }


init : Model
init =
    Model "" NotAsked Ui.Modal.init resetForm


resetForm : Form.Model
resetForm =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "New Password", 1, "New Password", "", Just "password", [] )
            , ( "Confirm Password", 2, "Confirm Password", "", Just "password", [] )
            ]
        , fileInputs = []
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }
