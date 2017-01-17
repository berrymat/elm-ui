module Roots.Actions.View exposing (..)

import Roots.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Roots.Edit.View as Edit
import Roots.Delete.View as Delete


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        DeleteModel subModel ->
            Html.map DeleteMsg (Delete.view token subModel)

        NoModel ->
            div [] []
