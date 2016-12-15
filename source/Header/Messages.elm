module Header.Messages exposing (..)

import Header.Models exposing (..)
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal


type Msg
    = HeaderResponse (WebData HeaderData)
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
    | EditModal Ui.Modal.Msg
    | OpenEditModal
    | CloseEditModal
    | DeleteModal Ui.Modal.Msg
    | OpenDeleteModal
    | CloseDeleteModal
