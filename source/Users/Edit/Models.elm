module Users.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Helpers.Helpers exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Users.Manager.User exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData User)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , user : User
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData User
    }


init : User -> HttpMethod -> Model
init user method =
    { id = user.id
    , user = user
    , method = method
    , form = userForm user
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


isAdministratorName : String
isAdministratorName =
    "Is Administrator?"


accessToCasesName : String
accessToCasesName =
    "Access to Cases?"


accessToMobilesName : String
accessToMobilesName =
    "Access to Mobiles?"


accessToStaffName : String
accessToStaffName =
    "Access to Staff?"


accessToClientsName : String
accessToClientsName =
    "Access to Clients?"


userForm : User -> Form.Model
userForm user =
    Form.init
        { checkboxes =
            [ ( isAdministratorName, 11, user.isAdministrator, [] )
            , ( accessToCasesName, 12, user.accessToCases, [] )
            , ( accessToMobilesName, 13, user.accessToMobiles, [] )
            , ( accessToStaffName, 14, user.accessToStaff, [] )
            , ( accessToClientsName, 15, user.accessToClients, [] )
            ]
        , inputs =
            [ ( "Email", 1, "Email", user.email, Nothing, [ Form.Validator requiredValidEmail ] )
            , ( "First Name", 2, "First Name", user.firstName, Nothing, [ Form.Validator requiredInput ] )
            , ( "Last Name", 3, "Last Name", user.lastName, Nothing, [ Form.Validator requiredInput ] )
            ]
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles =
            [ ( "access", 10, "Access - TODO" )
            ]
        }


updateUser : Form.Model -> User -> User
updateUser form user =
    { user
        | email = Form.valueOfInput "Email" user.email form
        , firstName = Form.valueOfInput "First Name" user.firstName form
        , lastName = Form.valueOfInput "Last Name" user.lastName form
        , isAdministrator = Form.valueOfCheckbox isAdministratorName user.isAdministrator form
        , accessToCases = Form.valueOfCheckbox accessToCasesName user.accessToCases form
        , accessToMobiles = Form.valueOfCheckbox accessToMobilesName user.accessToMobiles form
        , accessToStaff = Form.valueOfCheckbox accessToStaffName user.accessToStaff form
        , accessToClients = Form.valueOfCheckbox accessToClientsName user.accessToClients form
    }
