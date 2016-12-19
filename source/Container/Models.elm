module Container.Models exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (Tree, initialTree, Node)
import Header.Models exposing (..)
import Content.Models exposing (Content, initialContent)
import RemoteData exposing (..)


type alias Container =
    { authResult : WebData AuthResult
    , tree : WebData Tree
    , path : List Node
    , headerData : WebData HeaderData
    , headerUi : HeaderUi
    , tab : Tab
    , content : Content
    }


initialContainer : Container
initialContainer =
    { authResult = NotAsked
    , tree = NotAsked
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
