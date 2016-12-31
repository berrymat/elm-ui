module Content.Messages exposing (..)

import Http
import Content.Models exposing (..)
import Helpers.Models exposing (NodeId)
import Tree.Messages
import Table
import Ui.Modal


type ModalType
    = NewFolder


type ModalAction
    = Open
    | Save
    | Cancel


type FoldersMsg
    = OnFetchFiles NodeId (Result Http.Error (List File))
    | TreeMsg Tree.Messages.Msg
    | SetQuery String
    | SetTableState Table.State
    | ToggleFile NodeId
      -- MODALS
    | ModalAction ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg


type Msg
    = OnFetchFolders NodeId (Result Http.Error Folders)
    | OnFetchUsers NodeId (Result Http.Error Users)
    | OnFetchCases NodeId (Result Http.Error Cases)
    | OnFoldersMsg FoldersMsg
