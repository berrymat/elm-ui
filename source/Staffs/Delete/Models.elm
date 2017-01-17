module Staffs.Delete.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Staffs.Staff exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Staff)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , staff : Staff
    , modal : Ui.Modal.Model
    , response : WebData Staff
    }


init : Staff -> Model
init staff =
    { id = staff.id
    , staff = staff
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
