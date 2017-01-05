module Content.Models exposing (..)

import Helpers.Models exposing (..)
import Folders.Models


type Msg
    = FoldersMsg Folders.Models.Msg


type Content
    = FoldersContent Folders.Models.Folders
    | UsersContent Users
    | CasesContent Cases
    | EmptyContent


type alias Users =
    { id : NodeId
    , name : String
    }


type alias Cases =
    { id : NodeId
    , name : String
    }


initialContent : Content
initialContent =
    EmptyContent
