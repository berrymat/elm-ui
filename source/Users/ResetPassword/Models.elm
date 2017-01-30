module Users.ResetPassword.Models exposing (..)

import Helpers.Models exposing (..)
import Ui.Modal
import RemoteData exposing (..)
import Users.User exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData User)
    | ModalMsg Ui.Modal.Msg


type alias Model =
    { id : NodeId
    , user : User
    , method : HttpMethod
    , modal : Ui.Modal.Model
    , response : WebData User
    }


init : User -> Model
init user =
    { id = user.id
    , user = user
    , method = Put
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }
