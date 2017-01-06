module Folder.Models exposing (..)

import Helpers.Models exposing (..)
import Table
import Ui.DropdownMenu
import Ui.Modal
import Tree.Models exposing (Tree)
import Tree.Messages


type ModalType
    = MoveFiles
    | DeleteFiles
    | DownloadFiles


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = ToggleFile NodeId
    | SetQuery String
    | SetTableState Table.State
    | UpdateFolderInfo FolderInfo
    | UpdateMoveTree Tree
    | MoveTreeMsg Tree.Messages.Msg
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- PORTS
    | DownloadResponse String


type alias Folder =
    { info : FolderInfo
    , files : List File
    , tableState : Table.State
    , query : String
    , filesActionMenu : Ui.DropdownMenu.Model
    , filesMoveModal : Ui.Modal.Model
    , filesDeleteModal : Ui.Modal.Model
    , moveTree : Maybe Tree.Models.Tree
    }


type alias File =
    { id : NodeId
    , name : String
    , datetime : Float
    , writable : Bool
    , checked : Bool
    , url : String
    }


type alias FolderInfo =
    { id : NodeId
    , prefix : String
    , name : String
    , downloadUrl : String
    , isShared : Bool
    , isDeleted : Bool
    , isWritable : Bool
    , isReadable : Bool
    , isMovable : Bool
    , readableForCustomers : Bool
    , readableForClients : Bool
    , readableForStaff : Bool
    , writableForCustomers : Bool
    , writableForClients : Bool
    , writableForStaff : Bool
    }
