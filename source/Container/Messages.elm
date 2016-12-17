module Container.Messages exposing (..)

import Helpers.Models exposing (..)
import Tree.Messages
import Content.Messages
import RemoteData exposing (WebData)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


type ModalType
    = Edit
    | Delete


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = ShowContainer
    | AuthenticateResponse (WebData AuthResult)
    | SelectPath NodeId
    | SelectTab TabType
    | TreeMsg Tree.Messages.Msg
    | ContentMsg Content.Messages.Msg
    | HeaderResponse (WebData HeaderData)
    | HeaderPutResponse (WebData HeaderData)
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- EDIT FORM
    | EditFormMsg Form.Msg
