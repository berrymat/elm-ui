module Content.Models exposing (..)

import Helpers.Models exposing (..)
import Folders.Models
import Users.Models


type Msg
    = FoldersMsg Folders.Models.Msg
    | UsersMsg Users.Models.Msg


type Content
    = FoldersContent Folders.Models.Folders
    | UsersContent Users.Models.Model
    | CasesContent Cases
    | EmptyContent


type alias Cases =
    { id : NodeId
    , name : String
    }


initialContent : Content
initialContent =
    EmptyContent
