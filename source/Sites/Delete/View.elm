module Sites.Delete.View exposing (..)

import Sites.Delete.Models exposing (..)
import Helpers.Models exposing (..)
import Ui.Button
import Ui.Container
import Ui.Modal
import Html exposing (..)
import Html.Attributes exposing (..)


view : AuthToken -> Model -> Html Msg
view token model =
    let
        name =
            Maybe.withDefault "" model.site.name

        modalContent =
            [ div [ class "padded-modal-content" ]
                [ text ("Confirm deletion of site '" ++ name ++ "'?") ]
            ]

        modalViewModel =
            { content = modalContent
            , title = "Delete Site"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.danger "Delete" (Save token)
                    , Ui.Button.secondary "Cancel" Cancel
                    ]
                ]
            }
    in
        Ui.Modal.view ModalMsg modalViewModel model.modal
