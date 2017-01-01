module Content.Models exposing (..)

import Http
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Messages
import Table
import Ui.Button
import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)


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
      -- NEW FOLDER FORM
    | NewFolderFormMsg Form.Msg
    | FolderInfoPostResponse (WebData Folders)
    | FolderInfoPutResponse (WebData Folders)


type Msg
    = OnFetchFolders NodeId (Result Http.Error Folders)
    | OnFetchUsers NodeId (Result Http.Error Users)
    | OnFetchCases NodeId (Result Http.Error Cases)
    | OnFoldersMsg FoldersMsg


type Content
    = FoldersContent Folders
    | UsersContent Users
    | CasesContent Cases
    | EmptyContent


type alias Folders =
    { tree : Tree
    , newFolderButton : Ui.Button.Model
    , newFolderModal : Ui.Modal.Model
    , newFolderForm : Maybe (Form.Model Msg)
    , selected : Bool
    , path : List Node
    , folderId : NodeId
    , folderInfo : Maybe FolderInfo
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
    , name : String
    , isShared : Bool
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
