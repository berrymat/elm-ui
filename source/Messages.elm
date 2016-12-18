module Messages exposing (..)

import Ui.App
import Navigation exposing (Location)
import Container.Messages


type Msg
    = App Ui.App.Msg
    | ContainerMsg Container.Messages.Msg
    | OnLocationChange Location
