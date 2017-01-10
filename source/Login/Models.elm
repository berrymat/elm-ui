module Login.Models exposing (..)

import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)
import Helpers.Models exposing (..)


type Msg
    = AuthenticateResponse (WebData AuthResult)
    | LoginFormMsg Form.Msg
    | LoginModalMsg Ui.Modal.Msg
    | OpenLoginModal
    | SaveLoginModal
    | CancelLoginModal
    | GotoHome
    | LoadToken


type alias Login =
    { loginModal : Ui.Modal.Model
    , loginForm : Form.Model
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


loginForm : Form.Model
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
