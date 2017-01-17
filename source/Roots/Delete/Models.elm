module Roots.Delete.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Roots.Root exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Root)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , root : Root
    , modal : Ui.Modal.Model
    , response : WebData Root
    }


init : Root -> Model
init root =
    { id = root.id
    , root = root
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
