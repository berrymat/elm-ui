module Container.Messages exposing (..)

import Container.Models exposing (..)
import Tree.Models exposing (..)
import Header.Models exposing (..)
import Tree.Messages
import Content.Messages
import RemoteData exposing (WebData)
import Ui.DropdownMenu
import Ui.Modal


type Msg
    = ShowContainer
    | OnAuthenticate (WebData AuthResult)
    | SelectPath NodeId
    | SelectTab TabType
    | TreeMsg Tree.Messages.Msg
    | ContentMsg Content.Messages.Msg
    | HeaderResponse (WebData HeaderData)
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
    | EditModal Ui.Modal.Msg
    | OpenEditModal
    | CloseEditModal
    | DeleteModal Ui.Modal.Msg
    | OpenDeleteModal
    | CloseDeleteModal
