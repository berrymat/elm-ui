module Clients.Delete.View exposing (..)

import Clients.Delete.Models exposing (..)
import Helpers.Models exposing (..)
import Ui.Button
import Ui.Container
import Ui.Modal
import Html exposing (..)
import Html.Attributes exposing (..)


view : AuthToken -> Model -> Html Msg
view token model =
    let
        modalContent =
            [ div [ class "padded-modal-content" ]
                [ text ("Confirm deletion of client '" ++ model.client.name ++ "'?") ]
            ]

        modalViewModel =
            { content = modalContent
            , title = "Delete Client"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.danger "Delete" (Save token)
                    , Ui.Button.secondary "Cancel" Cancel
                    ]
                ]
            }
    in
        Ui.Modal.view ModalMsg modalViewModel model.modal
