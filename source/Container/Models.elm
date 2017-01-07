module Container.Models exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (Tree, initialTree, Node)
import Header.Models exposing (..)
import Content.Models exposing (Content, initialContent)
import RemoteData exposing (..)


type alias Container =
    { tree : WebData Tree
    , path : List Node
    , headerData : WebData HeaderData
    , headerUi : HeaderUi
    , childtypes : List Entity
    , tab : Tab
    , content : WebData Content
    }


initialContainer : Container
initialContainer =
    { tree = NotAsked
    , path = []
    , headerData = NotAsked
    , headerUi = initialHeaderUi
    , childtypes = []
    , tab = Tab EmptyTab ""
    , content = NotAsked
    }


type alias PathItem =
    { id : String
    , nodeType : NodeType
    , name : String
    }
