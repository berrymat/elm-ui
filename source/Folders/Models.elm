module Folders.Models exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Tree.Models
import Folder.Models
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import RemoteData exposing (..)
import Http exposing (..)
import Http.Progress as Progress exposing (Progress(..))
import Task exposing (Task)
import Ui.Native.FileManager


type ModalType
    = NewFolder
    | EditFolder
    | MoveFolder
    | DeleteFolder


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = MainTreeMsg Tree.Models.Msg
    | MoveTreeMsg Tree.Models.Msg
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction AuthToken ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- NEW FOLDER FORM
    | FolderFormMsg Form.Msg
    | FolderInfoSaveResponse (WebData Folders)
    | GetFolder ( String, Request Folder.Models.Folder )
    | GetFolderProgress (Progress Folder.Models.Folder)
      -- FOLDER MESSAGE
    | FolderMsg Folder.Models.Msg
    | UploadOpened AuthToken (Task Never (List Ui.Native.FileManager.File))
    | UploadGetFiles AuthToken (List Ui.Native.FileManager.File)
      --
    | UpdateFolder NodeId String


type alias Folders =
    { tree : Tree.Models.Tree
    , folderActionMenu : Ui.DropdownMenu.Model
    , folderEditMethod : Maybe HttpMethod
    , folderEditModal : Ui.Modal.Model
    , folderEditForm : Maybe Form.Model
    , folderMoveModal : Ui.Modal.Model
    , folderDeleteModal : Ui.Modal.Model
    , moveTree : Maybe Tree.Models.Tree
    , selected : Bool
    , path : List Tree.Models.Node
    , folderId : NodeId
    , folder : Progress Folder.Models.Folder
    , folderRequest : Maybe ( String, Request Folder.Models.Folder )
    }
