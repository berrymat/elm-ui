module Users.Models exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Table
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)
import Http


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
    | ModalAction ModalType ModalAction AuthToken
    | ModalMsg ModalType Ui.Modal.Msg
      -- EDIT USER FORM
    | UserFormMsg Form.Msg
    | UserSaveResponse (WebData Model)


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
    , userEditForm : Maybe (Form.Model Msg)
    , userChangePasswordModal : Ui.Modal.Model
    , userChangePasswordForm : Maybe (Form.Model Msg)
    , userDeleteModal : Ui.Modal.Model
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


initUser : NodeId -> User
initUser entityId =
    User entityId "" "" "" False False False False False False


usersUrl : NodeId -> String
usersUrl nodeId =
    apiUrl ++ "Users/" ++ nodeId


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


userForm : User -> Form.Model msg
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


updateUser : Form.Model msg -> User -> User
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
    let
        ( url, methodName ) =
            case method of
                Post ->
                    ( (apiUrl ++ "Users"), "POST" )

                Put ->
                    ( (apiUrl ++ "Users/" ++ user.id), "PUT" )
    in
        Http.request
            { method = methodName
            , url = url
            , headers = [ Http.header "X-CSRF-Token" token ]
            , body = (Http.jsonBody (encodeUser user))
            , expect = (Http.expectJson modelDecoder)
            , timeout = Nothing
            , withCredentials = True
            }
            |> Http.send (UserSaveResponse << RemoteData.fromResult)
