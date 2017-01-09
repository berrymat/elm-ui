module Helpers.RemoteData exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (..)
import Helpers.Models exposing (AuthToken)


view : WebData a -> (a -> Html msg) -> (String -> Html msg) -> Html msg
view webdata viewSuccess viewPending =
    case webdata of
        NotAsked ->
            (viewPending "fa fa-spin fa-spinner")

        Loading ->
            (viewPending "fa fa-spin fa-refresh")

        Failure err ->
            (viewPending "fa fa-exclamation-triangle")

        Success data ->
            (viewSuccess data)


viewToken : WebData a -> AuthToken -> (a -> AuthToken -> Html msg) -> (String -> Html msg) -> Html msg
viewToken webdata token viewSuccess viewPending =
    case webdata of
        NotAsked ->
            (viewPending "fa fa-spin fa-spinner")

        Loading ->
            (viewPending "fa fa-spin fa-refresh")

        Failure err ->
            (viewPending "fa fa-exclamation-triangle")

        Success data ->
            (viewSuccess data token)


viewPendingDefault : String -> String -> Html msg
viewPendingDefault containerClass iconClass =
    div [ class containerClass ]
        [ div [ class "header-loading" ]
            [ i [ class iconClass ] [] ]
        ]
