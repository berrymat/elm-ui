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
    | ModalAction ModalType ModalAction
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
    }


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , checked : Bool
    }


initUser : NodeId -> User
initUser entityId =
    User entityId "" "" "" False


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


userDecoder : Decode.Decoder User
userDecoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> hardcoded False


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "email", Encode.string user.email )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        ]


userForm : User -> Form.Model msg
userForm user =
    Form.init
        { checkboxes =
            []
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
    }


saveUser : User -> HttpMethod -> Cmd Msg
saveUser user method =
    let
        url =
            case method of
                Post ->
                    (apiUrl ++ "Users")

                Put ->
                    (apiUrl ++ "Users/" ++ user.id)
    in
        requester url
            method
            (encodeUser user)
            modelDecoder
            (UserSaveResponse << RemoteData.fromResult)
