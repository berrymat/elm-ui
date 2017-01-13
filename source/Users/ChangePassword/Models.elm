module Users.ChangePassword.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Ui.Input
import Ui.Modal
import RemoteData exposing (..)
import Char
import Users.Manager.User exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData User)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , changePassword : ChangePassword
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData User
    }


init : ChangePassword -> Model
init changePassword =
    { id = changePassword.id
    , changePassword = changePassword
    , form = changePasswordForm changePassword
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


validatePasswordComplexity : Form.Model -> Ui.Input.Model -> ValidationError
validatePasswordComplexity model input =
    let
        length =
            String.length input.value

        uppersCount =
            String.filter Char.isUpper input.value |> String.length

        lowersCount =
            String.filter Char.isLower input.value |> String.length

        digitsCount =
            String.filter Char.isDigit input.value |> String.length

        othersCount =
            length - (uppersCount + lowersCount + digitsCount)
    in
        if length < 8 || uppersCount == 0 || lowersCount == 0 || digitsCount == 0 || othersCount == 0 then
            Just "Password must be at least 8 characters long and include at least one uppercase, lowercase, numeric and punctuation character"
        else
            Nothing


matchingPasswords : Form.Model -> Ui.Input.Model -> ValidationError
matchingPasswords model input =
    let
        password =
            Form.valueOfInput "Password" "" model
    in
        if password /= input.value then
            Just "Passwords must match each other"
        else
            Nothing


changePasswordForm : ChangePassword -> Form.Model
changePasswordForm changePassword =
    Form.init
        { checkboxes = []
        , inputs =
            [ ( "Password", 1, "Password", changePassword.password, Just "password", [ Form.Validator validatePasswordComplexity ] )
            , ( "Confirm Password", 2, "Confirm Password", changePassword.confirmPassword, Just "password", [ Form.Validator matchingPasswords ] )
            ]
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles = []
        }


updateChangePassword : Form.Model -> ChangePassword -> ChangePassword
updateChangePassword form changePassword =
    { changePassword
        | password = Form.valueOfInput "Password" changePassword.password form
        , confirmPassword = Form.valueOfInput "Confirm Password" changePassword.confirmPassword form
    }
