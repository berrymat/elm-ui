module Roots.Edit.View exposing (..)

import Roots.Edit.Models exposing (..)
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

        ( title, saveText ) =
            case model.method of
                Post ->
                    ( "New Root", "Create" )

                Put ->
                    ( "Edit Root", "Update" )

                Delete ->
                    ( "", "" )

        modalViewModel =
            { content = modalContent
            , title = title
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary saveText (Save token)
                    , Ui.Button.secondary "Cancel" Cancel
                    ]
                ]
            }
    in
        Ui.Modal.view ModalMsg modalViewModel model.modal
