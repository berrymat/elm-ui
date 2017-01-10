module Models exposing (..)

import Ui.App
import Login.Models exposing (Login, initialLogin)
import Container.Models exposing (Container, initialContainer)
import Routing
import RemoteData exposing (..)
import Ui.NotificationCenter
import Navigation exposing (Location)
import Json.Decode


type Msg
    = App Ui.App.Msg
    | LoginMsg Login.Models.Msg
    | ContainerMsg Container.Models.Msg
    | OnLocationChange Location
    | Logout
    | LogoutResponse (WebData Bool)
    | HandleNotification Json.Decode.Value
    | NotificationMsg Ui.NotificationCenter.Msg


type alias Model =
    { app : Ui.App.Model
    , signedIn : WebData Bool
    , login : Login
    , container : Container
    , notificationCenter : Ui.NotificationCenter.Model Msg
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { app = Ui.App.init
    , signedIn = NotAsked
    , login = initialLogin
    , container = initialContainer
    , notificationCenter = Ui.NotificationCenter.init 30000 500
    , route = route
    }
