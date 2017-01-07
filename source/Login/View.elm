module Login.View exposing (..)

import Html exposing (..)
import Login.Models exposing (..)
import Ui.Button
import Ui.Container
import Ui.Modal
import Components.Form as Form


view : Login -> Html Msg
view login =
    let
        modalContent =
            [ Form.view LoginFormMsg login.loginForm ]

        loginModalViewModel =
            { content = modalContent
            , title = "Login"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Login" SaveLoginModal
                      --, Ui.Button.secondary "Cancel" CancelLoginModal
                    ]
                ]
            }
    in
        form []
            [ Ui.Modal.view LoginModalMsg loginModalViewModel login.loginModal ]
