module Helpers.Models exposing (..)

import Json.Encode as Encode


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


type HttpMethod
    = Post
    | Put
    | Delete


convertAccessType : String -> AccessType
convertAccessType type_ =
    if type_ == "r" then
        Read
    else if type_ == "w" then
        Write
    else
        None


type alias ChangePassword =
    { id : String
    , password : String
    , confirmPassword : String
    }


initChangePassword : NodeId -> ChangePassword
initChangePassword entityId =
    ChangePassword entityId "" ""



{-
   changePasswordDecoder : Decode.Decoder ChangePassword
   changePasswordDecoder =
       decode ChangePassword
           |> required "id" Decode.string
           |> required "password" Decode.string
           |> required "confirmPassword" Decode.string
-}


encodeChangePassword : ChangePassword -> Encode.Value
encodeChangePassword changePassword =
    Encode.object
        [ ( "id", Encode.string changePassword.id )
        , ( "password", Encode.string changePassword.password )
        , ( "confirmPassword", Encode.string changePassword.confirmPassword )
        ]
