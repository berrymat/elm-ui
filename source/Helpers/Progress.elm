module Helpers.Progress exposing (..)

import Http.Progress as Progress exposing (Progress(..))
import Html exposing (..)
import Html.Attributes exposing (..)


map : (a -> b) -> Progress a -> Progress b
map fn progress =
    case progress of
        Done data ->
            Done (fn data)

        None ->
            None

        Some progress ->
            Some progress

        Fail error ->
            Fail error


withDefault : a -> Progress a -> a
withDefault default progress =
    case progress of
        Done data ->
            data

        _ ->
            default


view : Progress a -> (a -> List (Html msg)) -> (String -> List (Html msg)) -> List (Html msg)
view progress viewSuccess viewPending =
    case progress of
        None ->
            (viewPending "fa fa-spin fa-spinner")

        Some progress ->
            let
                x =
                    Debug.log "progress" progress
            in
                (viewPending "fa fa-spin fa-refresh")

        Fail err ->
            (viewPending "fa fa-exclamation-triangle")

        Done data ->
            (viewSuccess data)


viewPendingDefault : String -> List (Html msg)
viewPendingDefault iconClass =
    [ div [ class "header-loading" ]
        [ i [ class iconClass ] [] ]
    ]
