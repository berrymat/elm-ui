module Staffs.Actions.View exposing (..)

import Staffs.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Staffs.Edit.View as Edit
import Staffs.Delete.View as Delete


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        DeleteModel subModel ->
            Html.map DeleteMsg (Delete.view token subModel)

        NoModel ->
            div [] []
