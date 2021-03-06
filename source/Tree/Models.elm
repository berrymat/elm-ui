module Tree.Models exposing (..)

import Helpers.Models exposing (..)
import RemoteData exposing (..)


type Msg
    = OnFetchRoot (WebData TempRoot)
    | OnFetchNode NodeId (WebData TempChildren)
    | ToggleNode NodeId
    | SelectRoot
    | SelectNode NodeId
    | OpenNewRoot NodeType NodeId
    | InsertNode NodeId NodeId NodeType ChildrenState String
    | UpdateNode NodeId String
    | NoAction


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


type Config msg
    = Config
        { treeMsg : Msg -> msg
        , selectedMsg : Msg -> msg
        , openRootMsg : ( NodeType, NodeId ) -> Msg -> msg
        }


config :
    { treeMsg : Msg -> msg
    , selectedMsg : Msg -> msg
    , openRootMsg : ( NodeType, NodeId ) -> Msg -> msg
    }
    -> Config msg
config { treeMsg, selectedMsg, openRootMsg } =
    Config
        { treeMsg = treeMsg
        , selectedMsg = selectedMsg
        , openRootMsg = openRootMsg
        }
