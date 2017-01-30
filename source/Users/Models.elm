module Users.Models exposing (..)

import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Table
import Ui.DropdownMenu
import Users.Actions.Models as Actions
import Users.User exposing (..)


type Msg
    = ToggleUser NodeId
    | SetQuery String
    | SetTableState Table.State
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | OpenModal AuthToken Actions.ModalType
    | ActionsMsg Actions.Msg


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
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


initUser : NodeId -> User
initUser entityId =
    User entityId "" "" "" False False False False False False


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
        |> hardcoded Actions.init
