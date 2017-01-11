module Users.Models exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Table
import Ui.DropdownMenu
import Ui.Input
import Ui.Modal
import Components.Form as Form exposing (ValidationError)
import RemoteData exposing (..)
import Char


type ModalType
    = NewUser
    | EditUser
    | RestrictUser
    | ResetPasswordUser
    | ChangePasswordUser
    | DeleteUser


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = ToggleUser NodeId
    | SetQuery String
    | SetTableState Table.State
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction AuthToken ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- EDIT USER FORM
    | UserFormMsg Form.Msg
    | UserSaveResponse (WebData Model)
      -- CHANGE PASSWORD FORM
    | UserChangePasswordFormMsg Form.Msg


type alias Model =
    { id : NodeId
    , canAdd : Bool
    , canEdit : Bool
    , canRestrict : Bool
    , canResetPassword : Bool
    , canChangePassword : Bool
    , canDelete : Bool
    , users : List User
    , tableState : Table.State
    , query : String
    , usersActionMenu : Ui.DropdownMenu.Model
    , userEditMethod : Maybe HttpMethod
    , userEditModal : Ui.Modal.Model
    , userEditForm : Maybe Form.Model
    , userDeleteModal : Ui.Modal.Model
    , userChangePasswordModal : Ui.Modal.Model
    , userChangePasswordForm : Maybe Form.Model
    , userResetPasswordModal : Ui.Modal.Model
    }


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , isAdministrator : Bool
    , accessToCases : Bool
    , accessToMobiles : Bool
    , accessToStaff : Bool
    , accessToClients : Bool
    , checked : Bool
    }


type alias ChangePassword =
    { id : String
    , password : String
    , confirmPassword : String
    }


initUser : NodeId -> User
initUser entityId =
    User entityId "" "" "" False False False False False False


initChangePassword : NodeId -> ChangePassword
initChangePassword entityId =
    ChangePassword entityId "" ""


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "canAdd" Decode.bool
        |> required "canEdit" Decode.bool
        |> required "canRestrict" Decode.bool
        |> required "canResetPassword" Decode.bool
        |> required "canChangePassword" Decode.bool
        |> required "canDelete" Decode.bool
        |> required "users" (Decode.list userDecoder)
        |> hardcoded (Table.initialSort "Email")
        |> hardcoded ""
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Nothing
        |> hardcoded Ui.Modal.init
        |> hardcoded Nothing
        |> hardcoded Ui.Modal.init
        |> hardcoded Ui.Modal.init
        |> hardcoded Nothing
        |> hardcoded Ui.Modal.init


userDecoder : Decode.Decoder User
userDecoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "isAdministrator" Decode.bool
        |> required "accessToCases" Decode.bool
        |> required "accessToMobiles" Decode.bool
        |> required "accessToStaff" Decode.bool
        |> required "accessToClients" Decode.bool
        |> hardcoded False


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "email", Encode.string user.email )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "isAdministrator", Encode.bool user.isAdministrator )
        , ( "accessToCases", Encode.bool user.accessToCases )
        , ( "accessToMobiles", Encode.bool user.accessToMobiles )
        , ( "accessToStaff", Encode.bool user.accessToStaff )
        , ( "accessToClients", Encode.bool user.accessToClients )
        ]


changePasswordDecoder : Decode.Decoder ChangePassword
changePasswordDecoder =
    decode ChangePassword
        |> required "id" Decode.string
        |> required "password" Decode.string
        |> required "confirmPassword" Decode.string


encodeChangePassword : ChangePassword -> Encode.Value
encodeChangePassword changePassword =
    Encode.object
        [ ( "id", Encode.string changePassword.id )
        , ( "password", Encode.string changePassword.password )
        , ( "confirmPassword", Encode.string changePassword.confirmPassword )
        ]


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
            [ ( isAdministratorName, 11, user.isAdministrator )
            , ( accessToCasesName, 12, user.accessToCases )
            , ( accessToMobilesName, 13, user.accessToMobiles )
            , ( accessToStaffName, 14, user.accessToStaff )
            , ( accessToClientsName, 15, user.accessToClients )
            ]
        , inputs =
            [ ( "Email", 1, "Email", user.email, Nothing )
            , ( "First Name", 2, "First Name", user.firstName, Nothing )
            , ( "Last Name", 3, "Last Name", user.lastName, Nothing )
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


saveUser : AuthToken -> User -> HttpMethod -> Cmd Msg
saveUser token user method =
    Helpers.Helpers.requester token "Users" user.id method (encodeUser user) modelDecoder (UserSaveResponse << RemoteData.fromResult)


validatePasswordComplexity : Form.Model -> Ui.Input.Model -> ValidationError
validatePasswordComplexity model input =
    let
        uppers =
            String.filter Char.isUpper input.value

        lowers =
            String.filter Char.isLower input.value
    in
        if
            (String.length input.value)
                < 8
                || (String.length uppers)
                < 1
                || (String.length lowers)
                < 1
        then
            Just "Password must be at least 8 characters long and include at least one uppercase, lowercase, numeric and punctuation character"
        else
            Nothing


changePasswordForm : ChangePassword -> Form.Model
changePasswordForm changePassword =
    Form.initEx
        { checkboxes = []
        , inputs =
            [ ( "Password", 1, "Password", changePassword.password, Just "password", [ Form.Validator validatePasswordComplexity ] )
            , ( "Confirm Password", 2, "Confirm Password", changePassword.confirmPassword, Just "password", [] )
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


saveChangePassword : AuthToken -> ChangePassword -> HttpMethod -> Cmd Msg
saveChangePassword token changePassword method =
    Helpers.Helpers.requester token "ChangePassword" changePassword.id method (encodeChangePassword changePassword) modelDecoder (UserSaveResponse << RemoteData.fromResult)
