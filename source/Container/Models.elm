module Container.Models exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (Tree, initialTree, Node)
import Header.Models exposing (..)
import Content.Models exposing (Content, initialContent)
import RemoteData exposing (..)


type alias Container =
    { tree : Tree
    , path : List Node
    , headerData : WebData HeaderData
    , headerUi : HeaderUi
    , tab : Tab
    , content : Content
    }


initialContainer : Container
initialContainer =
    { tree = initialTree
    , path = []
    , headerData = NotAsked
    , headerUi = initialHeaderUi
    , tab = Tab EmptyTab ""
    , content = initialContent
    }


type alias PathItem =
    { id : String
    , nodeType : NodeType
    , name : String
    }
