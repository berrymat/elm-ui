module Clients.Delete.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Clients.Client exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Client)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , client : Client
    , modal : Ui.Modal.Model
    , response : WebData Client
    }


init : Client -> Model
init client =
    { id = client.id
    , client = client
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
