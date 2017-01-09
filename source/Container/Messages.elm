module Container.Messages exposing (..)

import Helpers.Models exposing (..)
import Tree.Messages
import Content.Models
import Folders.Models
import Users.Models
import RemoteData exposing (WebData)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


type ModalType
    = EditHeader
    | DeleteHeader


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = GotoHome
    | Goto NodeType NodeId
    | LoadContainer NodeType NodeId NodeType
    | SelectPath NodeId
    | SelectTab TabType
    | TreeMsg Tree.Messages.Msg
    | ContentMsg Content.Models.Msg
    | HeaderResponse Bool (WebData HeaderData)
    | HeaderPutResponse (WebData HeaderData)
    | FetchFoldersResponse NodeId (WebData Folders.Models.Folders)
    | FetchUsersResponse NodeId (WebData Users.Models.Model)
    | FetchCasesResponse NodeId (WebData Content.Models.Cases)
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction AuthToken ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- EDIT FORM
    | EditFormMsg Form.Msg
