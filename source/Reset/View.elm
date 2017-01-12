module Reset.View exposing (..)

import Reset.Models exposing (..)
import Html exposing (..)
import Helpers.Models exposing (..)


view : AuthToken -> Model -> Html Msg
view token model =
    div [] [ text "Reset" ]
