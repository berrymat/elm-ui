module Folder.Models exposing (..)

import Helpers.Models exposing (..)
import Table


type Msg
    = ToggleFile NodeId
    | SetQuery String
    | SetTableState Table.State
    | UpdateFolderInfo FolderInfo


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
