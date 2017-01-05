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

        Some info ->
            Some info

        Fail error ->
            Fail error


withDefault : a -> Progress a -> a
withDefault default progress =
    case progress of
        Done data ->
            data

        _ ->
            default


view : Progress a -> (a -> Html msg) -> (String -> Html msg) -> Html msg
view progress viewSuccess viewPending =
    case progress of
        None ->
            (viewPending "fa fa-spin fa-spinner")

        Some info ->
            let
                x =
                    Debug.log "progress" info
            in
                (viewPending "fa fa-spin fa-refresh")

        Fail err ->
            (viewPending "fa fa-exclamation-triangle")

        Done data ->
            (viewSuccess data)


viewPendingDefault : String -> String -> Html msg
viewPendingDefault containerClass iconClass =
    div [ class containerClass ]
        [ div [ class "header-loading" ]
            [ i [ class iconClass ] [] ]
        ]


update : (a -> ( b, Cmd c )) -> Progress a -> ( Progress b, Cmd c )
update f progress =
    case progress of
        Done data ->
            let
                ( first, second ) =
                    f data
            in
                ( Done first, second )

        None ->
            ( None, Cmd.none )

        Some info ->
            ( Some info, Cmd.none )

        Fail error ->
            ( Fail error, Cmd.none )
