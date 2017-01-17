module Issues.Actions.View exposing (..)

import Issues.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Issues.Edit.View as Edit


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        NoModel ->
            div [] []
