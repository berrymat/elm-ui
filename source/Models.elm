module Models exposing (..)

import Ui.App
import Container.Models exposing (Container, initialContainer)
import Routing


type alias Model =
    { app : Ui.App.Model
    , container : Container
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { app = Ui.App.init
    , container = initialContainer
    , route = route
    }
