module Tree.Models exposing (..)

import Helpers.Models exposing (..)
import RemoteData exposing (..)


type alias TempNode =
    { id : NodeId
    , type_ : String
    , name : String
    , hasChildren : Bool
    , isRoot : Bool
    , rootType : String
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
    , rootType : NodeType
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
