module Tree.Messages exposing (..)

import Http
import Helpers.Models exposing (..)
import Tree.Models exposing (..)


type Msg
    = OnFetchRoot (Result Http.Error TempRoot)
    | OnFetchNode NodeId (Result Http.Error TempChildren)
    | ToggleNode NodeId
    | SelectRoot
    | SelectNode NodeId
    | SelectNewRoot NodeType NodeId
