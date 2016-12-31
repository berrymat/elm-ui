module Content.Models exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (Tree, Node)
import Table
import Ui.Button
import Ui.Modal


type Content
    = FoldersContent Folders
    | UsersContent Users
    | CasesContent Cases
    | EmptyContent


type alias Folders =
    { tree : Tree
    , newFolderButton : Ui.Button.Model
    , newFolderModal : Ui.Modal.Model
    , selected : Bool
    , path : List Node
    , folderId : NodeId
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


initialContent : Content
initialContent =
    EmptyContent
