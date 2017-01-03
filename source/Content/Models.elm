module Content.Models exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Messages
import Table
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)


type ModalType
    = NewFolder
    | EditFolder
    | MoveFolder
    | DeleteFolder


type ModalAction
    = Open
    | Save
    | Cancel


type FoldersMsg
    = FetchFolderResponse NodeId (WebData Folder)
    | MainTreeMsg Tree.Messages.Msg
    | MoveTreeMsg Tree.Messages.Msg
    | SetQuery String
    | SetTableState Table.State
    | ToggleFile NodeId
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- NEW FOLDER FORM
    | FolderFormMsg Form.Msg
    | FolderInfoSaveResponse (WebData Folders)


type Msg
    = FetchFoldersResponse NodeId (WebData Folders)
    | FetchUsersResponse NodeId (WebData Users)
    | FetchCasesResponse NodeId (WebData Cases)
    | OnFoldersMsg FoldersMsg


type Content
    = FoldersContent Folders
    | UsersContent Users
    | CasesContent Cases
    | EmptyContent


type alias Folders =
    { tree : Tree
    , folderActionMenu : Ui.DropdownMenu.Model
    , folderEditMethod : Maybe HttpMethod
    , folderEditModal : Ui.Modal.Model
    , folderEditForm : Maybe (Form.Model Msg)
    , folderMoveModal : Ui.Modal.Model
    , folderDeleteModal : Ui.Modal.Model
    , moveTree : Maybe Tree
    , selected : Bool
    , path : List Node
    , folderId : NodeId
    , folder : WebData Folder
    }


type alias Folder =
    { info : FolderInfo
    , files : List File
    , tableState : Table.State
    , query : String
    }


type alias File =
    { id : NodeId
    , name : String
    , datetime : Float
    , writable : Bool
    , checked : Bool
    }


type alias Users =
    { id : NodeId
    , name : String
    }


type alias Cases =
    { id : NodeId
    , name : String
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


initialContent : Content
initialContent =
    EmptyContent
