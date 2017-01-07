module Models exposing (..)

import Ui.App
import Login.Models exposing (Login, initialLogin)
import Container.Models exposing (Container, initialContainer)
import Routing
import RemoteData exposing (..)


type alias Model =
    { app : Ui.App.Model
    , signedIn : WebData Bool
    , login : Login
    , container : Container
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { app = Ui.App.init
    , signedIn = NotAsked
    , login = initialLogin
    , container = initialContainer
    , route = route
    }
