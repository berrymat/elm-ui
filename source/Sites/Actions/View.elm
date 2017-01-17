module Sites.Actions.View exposing (..)

import Sites.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Sites.Edit.View as Edit
import Sites.Delete.View as Delete


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        DeleteModel subModel ->
            Html.map DeleteMsg (Delete.view token subModel)

        NoModel ->
            div [] []
