module Messages exposing (..)

import Ui.App
import Navigation exposing (Location)
import Login.Models
import Container.Messages
import RemoteData exposing (..)


type Msg
    = App Ui.App.Msg
    | LoginMsg Login.Models.Msg
    | ContainerMsg Container.Messages.Msg
    | OnLocationChange Location
    | Logout
    | LogoutResponse (WebData Bool)
