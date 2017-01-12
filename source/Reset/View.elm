module Reset.View exposing (..)

import Reset.Models exposing (..)
import Html exposing (..)
import Helpers.Models exposing (..)
import Ui.Button
import Ui.Container
import Ui.Modal
import Components.Form as Form


view : AuthToken -> Model -> Html Msg
view token model =
    let
        modalContent =
            [ Form.view ResetFormMsg model.resetForm ]

        resetModalViewModel =
            { content = modalContent
            , title = "Reset Password"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Reset" (SaveResetModal token)
                      --, Ui.Button.secondary "Cancel" CancelLoginModal
                    ]
                ]
            }
    in
        form []
            [ Ui.Modal.view ResetModalMsg resetModalViewModel model.resetModal ]
