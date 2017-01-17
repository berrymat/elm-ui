module Sites.Delete.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Sites.Site exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Site)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , site : Site
    , modal : Ui.Modal.Model
    , response : WebData Site
    }


init : Site -> Model
init site =
    { id = site.id
    , site = site
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
