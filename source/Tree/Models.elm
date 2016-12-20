module Tree.Models exposing (..)

import Helpers.Models exposing (..)
import RemoteData exposing (..)


type alias TempNode =
    { id : NodeId
    , type_ : String
    , name : String
    , hasChildren : Bool
    , isRoot : Bool
    }


type alias TempChildren =
    { id : NodeId
    , type_ : String
    , children : List TempNode
    }


type alias TempRoot =
    { id : NodeId
    , type_ : String
    , name : String
    , children : List TempNode
    }


type ChildrenState
    = NoChildren
    | Collapsed
    | Expanding
    | Expanded
    | RootNode


type alias Node =
    { id : NodeId
    , nodeType : NodeType
    , name : String
    , selected : Bool
    , childrenState : ChildrenState
    , childNodes : WebData ChildNodes
    }


type ChildNodes
    = ChildNodes (List Node)


type alias Tree =
    { id : NodeId
    , nodeType : NodeType
    , name : String
    , selected : Bool
    , childrenState : ChildrenState
    , childNodes : ChildNodes
    , path : List Node
    }


convertNodeType : String -> Maybe NodeType
convertNodeType type_ =
    let
        lowerType =
            String.toLower type_
    in
        if lowerType == "root" then
            Just RootType
        else if lowerType == "customer" then
            Just CustomerType
        else if lowerType == "client" then
            Just ClientType
        else if lowerType == "site" then
            Just SiteType
        else if lowerType == "staff" then
            Just StaffType
        else if lowerType == "folder" then
            Just FolderType
        else
            Nothing


initialTree : Tree
initialTree =
    { id = ""
    , nodeType = RootType
    , name = ""
    , selected = True
    , childrenState = NoChildren
    , childNodes = ChildNodes []
    , path = []
    }
