module Tree.Messages exposing (..)

import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import RemoteData exposing (WebData)


type Msg
    = OnFetchRoot (WebData TempRoot)
    | OnFetchNode NodeId (WebData TempChildren)
    | ToggleNode NodeId
    | SelectRoot
    | SelectNode NodeId
    | OpenNewRoot NodeType NodeId
    | UpdateNode NodeId String
    | NoAction
