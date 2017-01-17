module Main exposing (..)

import Models exposing (..)
import Navigation exposing (Location)
import Routing exposing (Route)
import Update exposing (update, fetchData)
import View exposing (view)
import Container.Update
import Ui.Helpers.Emitter


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location

        model =
            initialModel currentRoute
    in
        fetchData model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ContainerMsg (Container.Update.subscriptions model.container)
        , Ui.Helpers.Emitter.listen "notificationChannel" HandleNotification
        ]


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
