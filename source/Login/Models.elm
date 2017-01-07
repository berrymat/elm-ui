module Login.Models exposing (..)

import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)
import Helpers.Models exposing (..)


type Msg
    = LoadLogin
    | AuthenticateResponse (WebData AuthResult)
    | LoginFormMsg Form.Msg
    | LoginModalMsg Ui.Modal.Msg
    | SaveLoginModal
    | CancelLoginModal


type alias Login =
    { loginModal : Ui.Modal.Model
    , loginForm : Form.Model Msg
    , authResult : WebData AuthResult
    }


initialLogin : Login
initialLogin =
    { loginModal = Ui.Modal.init
    , loginForm = loginForm
    , authResult = NotAsked
    }


rememberMe : String
rememberMe =
    "Remember me?"


loginForm : Form.Model Msg
loginForm =
    let
        checkboxes =
            [ ( rememberMe, 11, False )
            ]

        inputs =
            [ ( "Email", 1, "Email address", "", Just "email" )
            , ( "Password", 2, "Password", "", Just "password" )
            ]
    in
        Form.init
            { checkboxes = checkboxes
            , inputs = inputs
            , numberRanges = []
            , textareas = []
            , choosers = []
            , colors = []
            , dates = []
            , titles = []
            }
