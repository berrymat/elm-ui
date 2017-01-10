module Helpers.Models exposing (..)


type alias NodeId =
    String


type alias AuthToken =
    String


type NodeType
    = RootType
    | CustomerType
    | ClientType
    | SiteType
    | StaffType
    | FolderType


type alias AuthResult =
    { nodeType : NodeType
    , nodeId : NodeId
    , result : String
    , authToken : String
    , childtypes : List Entity
    }


type alias Notification =
    { notificationType : String
    , message : String
    }


type TabType
    = FoldersType
    | UsersType
    | CasesType
    | EmptyTab



type alias Tab =
    { tabType : TabType
    , name : String
    }


type alias Entity =
    { nodeType : NodeType
    , name : String
    }


type alias UserAccess =
    { admin : Bool
    , owner : Bool
    , root : Bool
    }


type AccessType
    = None
    | Read
    | Write


convertAccessType : String -> AccessType
convertAccessType type_ =
    if type_ == "r" then
        Read
    else if type_ == "w" then
        Write
    else
        None




