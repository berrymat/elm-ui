module Users.ChangePassword.View exposing (..)

import Users.ChangePassword.Models exposing (..)
import Helpers.Models exposing (..)
import Components.Form as Form
import Ui.Button
import Ui.Container
import Ui.Modal
import Html exposing (..)


view : AuthToken -> Model -> Html Msg
view token model =
    let
        modalContent =
            [ Form.view FormMsg model.form ]

        modalViewModel =
            { content = modalContent
            , title = "Change Password"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Change" (Save token)
                    , Ui.Button.secondary "Cancel" Cancel
                    ]
                ]
            }
    in
        Ui.Modal.view ModalMsg modalViewModel model.modal
