module Container.Messages exposing (..)

import Helpers.Models exposing (..)
import Tree.Messages
import Content.Messages
import RemoteData exposing (WebData)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


type Msg
    = ShowContainer
    | OnAuthenticate (WebData AuthResult)
    | SelectPath NodeId
    | SelectTab TabType
    | TreeMsg Tree.Messages.Msg
    | ContentMsg Content.Messages.Msg
    | HeaderResponse (WebData HeaderData)
    | HeaderPutResponse (WebData HeaderData)
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
    | EditModal Ui.Modal.Msg
    | OpenEditModal
    | SaveEditModal
    | CloseEditModal
    | DeleteModal Ui.Modal.Msg
    | OpenDeleteModal
    | CloseDeleteModal
    | Form Form.Msg
