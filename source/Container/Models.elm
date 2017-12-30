module Container.Models exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (Tree, initialTree, Node)
import Header.Models
import Folders.Models
import Issues.Models
import Users.Models
import Content.Models exposing (Content, initialContent)
import RemoteData exposing (..)


type Msg
    = GotoHome
    | Goto NodeType NodeId
    | LoadContainer NodeType NodeId NodeType
    | SelectPath NodeId
    | SelectTab TabType
    | TreeMsg Tree.Models.Msg
    | HeaderMsg Header.Models.Msg
    | ContentMsg Content.Models.Msg
    | FetchHeaderResponse Bool (WebData Header.Models.Model)
    | FetchFoldersResponse NodeId (WebData Folders.Models.Folders)
    | FetchUsersResponse NodeId (WebData Users.Models.Model)
    | FetchIssuesResponse NodeId (WebData Issues.Models.Model)


type alias Container =
    { tree : WebData Tree
    , path : List Node
    , header : WebData Header.Models.Model
    , childtypes : List Entity
    , tab : Tab
    , content : WebData Content
    }


initialContainer : Container
initialContainer =
    { tree = NotAsked
    , path = []
    , header = NotAsked
    , childtypes = []
    , tab = Tab EmptyTab ""
    , content = NotAsked
    }


type alias PathItem =
    { id : String
    , nodeType : NodeType
    , name : String
    }


isHeaderEmpty : Container -> Bool
isHeaderEmpty container =
    (container.header == NotAsked)
