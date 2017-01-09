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


type Header
    = RootHeader Root
    | CustomerHeader Customer
    | ClientHeader Client
    | SiteHeader Site
    | StaffHeader Staff
    | Empty


type alias HeaderData =
    { header : Header
    , tabs : List Tab
    , childtypes : List Entity
    , useraccess : UserAccess
    }


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


type alias Root =
    { id : NodeId
    , access : RootAccess
    , values : RootValues
    }


type alias RootAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


type alias RootValues =
    { name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , contact : Maybe String
    , tel : Maybe String
    , email : Maybe String
    }


type alias Customer =
    { id : NodeId
    , access : CustomerAccess
    , values : CustomerValues
    }


customer : NodeId -> CustomerAccess -> CustomerValues -> Customer
customer id access values =
    Customer id access values


type alias CustomerAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


type alias CustomerValues =
    { name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , contact : Maybe String
    , tel : Maybe String
    , email : Maybe String
    }


type alias Client =
    { id : NodeId
    , access : ClientAccess
    , values : ClientValues
    }


type alias ClientValues =
    { no : Maybe String
    , name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , contact : Maybe String
    , tel : Maybe String
    , email : Maybe String
    }


type alias ClientAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


type alias Site =
    { id : NodeId
    , access : SiteAccess
    , values : SiteValues
    }


type alias SiteValues =
    { no : Maybe String
    , name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , contact : Maybe String
    , tel : Maybe String
    , email : Maybe String
    , divisionMgr : Maybe String
    , areaMgr : Maybe String
    , supervisor : Maybe String
    }


type alias SiteAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    , managers : AccessType
    }


type alias Staff =
    { id : NodeId
    , access : StaffAccess
    , values : StaffValues
    }


type alias StaffValues =
    { no : Maybe String
    , name : Maybe String
    , image : Maybe String
    , address1 : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , address4 : Maybe String
    , postcode : Maybe String
    , tel : Maybe String
    , mob : Maybe String
    , email : Maybe String
    }


type alias StaffAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }
