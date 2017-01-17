module Clients.Actions.View exposing (..)

import Clients.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Clients.Edit.View as Edit
import Clients.Delete.View as Delete


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        DeleteModel subModel ->
            Html.map DeleteMsg (Delete.view token subModel)

        NoModel ->
            div [] []
