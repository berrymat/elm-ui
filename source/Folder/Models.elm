module Folder.Models exposing (..)

import Helpers.Models exposing (..)
import Table
import Ui.DropdownMenu
import Ui.Modal
import Tree.Models


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
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg


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
    }


type alias FolderInfo =
    { id : NodeId
    , prefix : String
    , name : String
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
