module Container.Update exposing (..)

import Container.Messages exposing (Msg(..))
import Container.Commands exposing (..)
import Container.Models exposing (..)
import Tree.Messages
import Tree.Update
import Tree.Models exposing (..)
import Header.Models exposing (..)
import Header.Commands
import Content.Commands
import Content.Update
import Navigation
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


updatePathFromTree : Container -> Tree -> Cmd Tree.Messages.Msg -> List Node -> ( Container, Cmd Msg )
updatePathFromTree container updatedTree cmdTree path =
    let
        maybeSelected =
            List.head path

        ( headerId, headerType ) =
            case maybeSelected of
                Just selected ->
                    ( selected.id, selected.nodeType )

                Nothing ->
                    ( container.tree.id, container.tree.nodeType )

        ( newHeaderInfo, cmdHeader ) =
            Header.Commands.fetchHeader container.headerInfo headerType headerId

        cmdBatch =
            Cmd.batch
                [ Cmd.map TreeMsg cmdTree
                , cmdHeader
                ]
    in
        ( { container | tree = updatedTree, path = path, headerInfo = newHeaderInfo }, cmdBatch )


update : Msg -> Container -> ( Container, Cmd Msg )
update message container =
    let
        headerInfo =
            container.headerInfo

        ui =
            headerInfo.ui
    in
        case message of
            ShowContainer ->
                ( container
                , Navigation.newUrl "#container/customer/path/Customer-46-Client"
                )

            OnAuthenticate result ->
                RemoteData.map (fetchIfAuthorized container) result
                    |> RemoteData.withDefault ( container, Cmd.none )

            SelectPath nodeId ->
                let
                    ( updatedTree, cmdTree, path ) =
                        Tree.Update.update (Tree.Messages.SelectNode nodeId) container.tree
                in
                    updatePathFromTree container updatedTree cmdTree path

            SelectTab tabType ->
                let
                    nodeId =
                        Header.Models.headerId container.headerInfo

                    updatedTab =
                        getTabFromType container.headerInfo tabType

                    cmdContent =
                        Content.Commands.fetchContent tabType nodeId
                in
                    ( { container | tab = updatedTab }, Cmd.map ContentMsg cmdContent )

            TreeMsg subMsg ->
                let
                    ( updatedTree, cmdTree, path ) =
                        Tree.Update.update subMsg container.tree
                in
                    updatePathFromTree container updatedTree cmdTree path

            ContentMsg subMsg ->
                let
                    ( updatedContent, cmdContent ) =
                        Content.Update.update subMsg container.content
                in
                    ( { container | content = updatedContent }, Cmd.map ContentMsg cmdContent )

            HeaderResponse newHeaderData ->
                RemoteData.map (fetchContent container) newHeaderData
                    |> RemoteData.withDefault ( container, Cmd.none )

            HeaderPutResponse webdata ->
                let
                    x =
                        Debug.log "HeaderPutResponse Data" webdata

                    newContainer =
                        case webdata of
                            NotAsked ->
                                container

                            Loading ->
                                container

                            Failure err ->
                                let
                                    x =
                                        Debug.log ("Failure " ++ (toString err))
                                in
                                    container

                            Success data ->
                                let
                                    newEditModal =
                                        Ui.Modal.close ui.editModal

                                    newUi =
                                        { ui | editModal = newEditModal }
                                in
                                    { container | headerInfo = { headerInfo | ui = newUi, data = webdata } }
                in
                    ( newContainer, Cmd.none )

            ActionMenu action ->
                let
                    newActionMenu =
                        Ui.DropdownMenu.update action ui.actionMenu

                    newUi =
                        { ui | actionMenu = newActionMenu }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            CloseActionMenu ->
                let
                    newActionMenu =
                        Ui.DropdownMenu.close ui.actionMenu

                    newUi =
                        { ui | actionMenu = newActionMenu }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            NoAction ->
                ( container, Cmd.none )

            CloseEditModal ->
                let
                    newEditModal =
                        Ui.Modal.close ui.editModal

                    newUi =
                        { ui | editModal = newEditModal }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            SaveEditModal ->
                case ui.editForm of
                    Just form ->
                        let
                            {-
                               newEditModal =
                                   Ui.Modal.close ui.editModal

                               newUi =
                                   { ui | editModal = newEditModal }
                            -}
                            nodeId =
                                Header.Models.headerId headerInfo

                            newContainer =
                                RemoteData.map (updateState form container) container.headerInfo.data
                                    |> RemoteData.withDefault container

                            newEffect =
                                RemoteData.map (Header.Commands.putHeader nodeId) newContainer.headerInfo.data
                                    |> RemoteData.withDefault Cmd.none

                            {-
                               newContainer =
                                   { container | headerInfo = { headerInfo | ui = newUi } }
                            -}
                        in
                            ( newContainer, newEffect )

                    Nothing ->
                        ( container, Cmd.none )

            OpenEditModal ->
                let
                    newEditForm =
                        RemoteData.map (initEditForm container) headerInfo.data
                            |> RemoteData.withDefault ui.editForm

                    newEditModal =
                        Ui.Modal.open ui.editModal

                    newActionMenu =
                        Ui.DropdownMenu.close ui.actionMenu

                    newUi =
                        { ui | editModal = newEditModal, editForm = newEditForm, actionMenu = newActionMenu }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            EditModal action ->
                let
                    newEditModal =
                        Ui.Modal.update action ui.editModal

                    newUi =
                        { ui | editModal = newEditModal }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            CloseDeleteModal ->
                let
                    newDeleteModal =
                        Ui.Modal.close ui.deleteModal

                    newActionMenu =
                        Ui.DropdownMenu.close ui.actionMenu

                    newUi =
                        { ui | deleteModal = newDeleteModal, actionMenu = newActionMenu }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            OpenDeleteModal ->
                let
                    newDeleteModal =
                        Ui.Modal.open ui.deleteModal

                    newActionMenu =
                        Ui.DropdownMenu.close ui.actionMenu

                    newUi =
                        { ui | deleteModal = newDeleteModal, actionMenu = newActionMenu }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            DeleteModal action ->
                let
                    newDeleteModal =
                        Ui.Modal.update action ui.deleteModal

                    newUi =
                        { ui | deleteModal = newDeleteModal }
                in
                    ( { container | headerInfo = { headerInfo | ui = newUi } }, Cmd.none )

            Form act ->
                case ui.editForm of
                    Just form ->
                        let
                            ( newForm, effect ) =
                                Form.update act form

                            ( newContainer, newEffect ) =
                                ( { container | headerInfo = { headerInfo | ui = { ui | editForm = Just newForm } } }
                                , Cmd.map Form effect
                                )
                        in
                            ( newContainer, newEffect )

                    Nothing ->
                        ( container, Cmd.none )



{- TODO
   let
       updatedComponent input =
           { input
               | disabled = Form.valueOfCheckbox "disabled" False model.form
               , readonly = Form.valueOfCheckbox "readonly" False model.form
               , placeholder = Form.valueOfInput "placeholder" "" model.form
               , value = Form.valueOfInput "value" "" model.form
           }
   in
       ( { model | input = updatedComponent model.input }, effect )
-}


subscriptions : Container -> Sub Msg
subscriptions container =
    Sub.batch
        [ Sub.map ActionMenu (Ui.DropdownMenu.subscriptions container.headerInfo.ui.actionMenu)
        ]
