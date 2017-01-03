module Container.Update exposing (..)

import Container.Messages exposing (..)
import Container.Commands exposing (..)
import Container.Models exposing (..)
import Tree.Messages
import Tree.Update
import Tree.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Header.Models exposing (..)
import Header.Commands
import Content.Commands
import Content.Update
import Navigation
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


update : Msg -> Container -> ( Container, Cmd Msg )
update message container =
    let
        ui =
            container.headerUi
    in
        case message of
            GotoHome ->
                updateGotoHome container

            Goto nodeType nodeId ->
                updateGoto container nodeType nodeId

            LoadContainer parentType nodeId childType ->
                updateLoadContainer parentType nodeId childType container

            AuthenticateResponse result ->
                RemoteData.map (fetchIfAuthorized container) result
                    |> RemoteData.withDefault ( container, Cmd.none )

            SelectPath nodeId ->
                let
                    ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                        Tree.Update.update (Tree.Messages.SelectNode nodeId) container.tree
                in
                    updatePathFromTree container cmdTree maybePath maybeRoot updatedTree

            SelectTab tabType ->
                let
                    nodeId =
                        Header.Models.headerId container.headerData

                    updatedTab =
                        getTabFromType container.headerData tabType

                    cmdContent =
                        Content.Commands.fetchContent tabType nodeId
                in
                    ( { container | tab = updatedTab, content = Loading }, Cmd.map ContentMsg cmdContent )

            TreeMsg subMsg ->
                let
                    ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                        Tree.Update.update subMsg container.tree
                in
                    updatePathFromTree container cmdTree maybePath maybeRoot updatedTree

            ContentMsg subMsg ->
                let
                    ( updatedContent, cmdContent ) =
                        Content.Update.update subMsg container.content
                in
                    ( { container | content = updatedContent }, Cmd.map ContentMsg cmdContent )

            HeaderResponse isTree newHeaderData ->
                RemoteData.map (fetchContent container isTree) newHeaderData
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
                                    { container | headerUi = newUi, headerData = webdata }
                in
                    ( newContainer, Cmd.none )

            -- ACTION MENU
            ActionMenu action ->
                updateActionMenu container action

            CloseActionMenu ->
                updateCloseActionMenu container

            NoAction ->
                ( container, Cmd.none )

            -- EDIT MODAL
            ModalAction Edit action ->
                updateEditModalAction container action

            ModalMsg Edit msg ->
                updateEditModalMsg container msg

            -- DELETE MODAL
            ModalAction Delete action ->
                updateDeleteModalAction container action

            ModalMsg Delete msg ->
                updateDeleteModalMsg container msg

            -- FORM
            EditFormMsg msg ->
                updateEditFormMsg container msg


subscriptions : Container -> Sub Msg
subscriptions container =
    Sub.batch
        [ Sub.map ActionMenu (Ui.DropdownMenu.subscriptions container.headerUi.actionMenu)
        , Sub.map ContentMsg (Content.Update.subscriptions container.content)
        ]


updateGotoHome : Container -> ( Container, Cmd Msg )
updateGotoHome container =
    ( container, Navigation.newUrl "#Home" )


updateGoto : Container -> NodeType -> NodeId -> ( Container, Cmd Msg )
updateGoto container nodeType nodeId =
    ( container, goto nodeType nodeId )


goto : NodeType -> NodeId -> Cmd Msg
goto nodeType nodeId =
    let
        path =
            nodeTypeToPath nodeType
    in
        Navigation.newUrl ("#" ++ path ++ "/" ++ nodeId)


updateLoadContainer : NodeType -> NodeId -> NodeType -> Container -> ( Container, Cmd Msg )
updateLoadContainer parentType nodeId childType container =
    case container.authResult of
        NotAsked ->
            let
                cmd =
                    authenticate
                        "berry.matthew@me.com"
                        "Cirrus8914!"
            in
                ( { container | authResult = Loading }, cmd )

        Loading ->
            ( container, Cmd.none )

        Failure err ->
            ( container, Cmd.none )

        Success result ->
            let
                maybeTypes =
                    maybeAuthResultTypes result
            in
                case maybeTypes of
                    Just ( resultParentType, resultNodeId, resultChildType ) ->
                        let
                            parentType_ =
                                if nodeId == "" then
                                    resultParentType
                                else
                                    parentType

                            nodeId_ =
                                if nodeId == "" then
                                    resultNodeId
                                else
                                    nodeId

                            childType_ =
                                if nodeId == "" then
                                    resultChildType
                                else
                                    childType
                        in
                            fetchInitialData parentType_
                                nodeId_
                                childType_
                                container

                    Nothing ->
                        ( container, Cmd.none )


updatePathFromTree : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> ( Container, Cmd Msg )
updatePathFromTree container cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updatePathFromTreeSuccess container cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( container, Cmd.none )


updatePathFromTreeSuccess : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> ( Container, Cmd Msg )
updatePathFromTreeSuccess container cmdTree maybePath maybeRoot updatedTree =
    let
        x =
            Debug.log "updatePathFromTreeSuccess maybePath" maybePath

        y =
            Debug.log "updatePathFromTreeSuccess maybeRoot" maybeRoot

        ( newContainer, cmdHeader ) =
            case maybePath of
                Just path ->
                    List.head path
                        |> Maybe.map (\s -> ( s.nodeType, s.id, False ))
                        |> Maybe.withDefault ( updatedTree.nodeType, updatedTree.id, True )
                        |> Header.Commands.fetchHeader { container | path = path }

                Nothing ->
                    ( container, Cmd.none )

        cmdRoot =
            case maybeRoot of
                Just ( treeType, treeId ) ->
                    goto treeType treeId

                Nothing ->
                    Cmd.none

        cmdBatch =
            Cmd.batch
                [ Cmd.map TreeMsg cmdTree
                , cmdHeader
                , cmdRoot
                ]
    in
        ( { newContainer | tree = Success updatedTree }, cmdBatch )



-- ACTION MENU UPDATES


applyNewActionMenu : Container -> Ui.DropdownMenu.Model -> ( Container, Cmd Msg )
applyNewActionMenu container newMenu =
    let
        headerUi =
            container.headerUi
    in
        ( { container | headerUi = { headerUi | actionMenu = newMenu } }, Cmd.none )


updateActionMenu : Container -> Ui.DropdownMenu.Msg -> ( Container, Cmd Msg )
updateActionMenu container action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action container.headerUi.actionMenu
    in
        applyNewActionMenu container newActionMenu


updateCloseActionMenu : Container -> ( Container, Cmd Msg )
updateCloseActionMenu container =
    let
        newActionMenu =
            Ui.DropdownMenu.close container.headerUi.actionMenu
    in
        applyNewActionMenu container newActionMenu



-- HELPERS


applyActionMenu : Ui.DropdownMenu.Model -> HeaderUi -> HeaderUi
applyActionMenu newMenu headerUi =
    { headerUi | actionMenu = newMenu }


applyHeaderUi : HeaderUi -> Container -> Container
applyHeaderUi newUi container =
    { container | headerUi = newUi }



-- EDIT MODAL UPDATES


applyEditModal : Ui.Modal.Model -> HeaderUi -> HeaderUi
applyEditModal newModal headerUi =
    { headerUi | editModal = newModal }


applyEditForm : Maybe (Form.Model Msg) -> HeaderUi -> HeaderUi
applyEditForm newForm headerUi =
    { headerUi | editForm = newForm }


applyNewEditModal : Container -> Ui.Modal.Model -> Maybe (Form.Model Msg) -> Ui.DropdownMenu.Model -> ( Container, Cmd Msg )
applyNewEditModal container newModal newForm newMenu =
    ( applyHeaderUi
        (container.headerUi
            |> applyEditModal newModal
            |> applyEditForm newForm
            |> applyActionMenu newMenu
        )
        container
    , Cmd.none
    )


updateEditModalAction : Container -> ModalAction -> ( Container, Cmd Msg )
updateEditModalAction container action =
    case action of
        Open ->
            updateEditModalOpen container

        Save ->
            updateEditModalSave container

        Cancel ->
            updateEditModalCancel container


updateEditModalOpen : Container -> ( Container, Cmd Msg )
updateEditModalOpen container =
    let
        newEditForm =
            RemoteData.map (initEditForm container) container.headerData
                |> RemoteData.withDefault container.headerUi.editForm

        newEditModal =
            Ui.Modal.open container.headerUi.editModal

        newActionMenu =
            Ui.DropdownMenu.close container.headerUi.actionMenu
    in
        applyNewEditModal container newEditModal newEditForm newActionMenu


updateEditModalSave : Container -> ( Container, Cmd Msg )
updateEditModalSave container =
    case container.headerUi.editForm of
        Just form ->
            let
                nodeId =
                    Header.Models.headerId container.headerData

                newContainer =
                    RemoteData.map (updateState form container) container.headerData
                        |> RemoteData.withDefault container

                newEffect =
                    RemoteData.map (Header.Commands.putHeader nodeId) newContainer.headerData
                        |> RemoteData.withDefault Cmd.none
            in
                ( newContainer, newEffect )

        Nothing ->
            ( container, Cmd.none )


updateEditModalCancel : Container -> ( Container, Cmd Msg )
updateEditModalCancel container =
    let
        ui =
            container.headerUi

        newEditModal =
            Ui.Modal.close ui.editModal
    in
        applyNewEditModal container newEditModal ui.editForm ui.actionMenu


updateEditModalMsg : Container -> Ui.Modal.Msg -> ( Container, Cmd Msg )
updateEditModalMsg container msg =
    let
        ui =
            container.headerUi

        newEditModal =
            Ui.Modal.update msg ui.editModal
    in
        applyNewEditModal container newEditModal ui.editForm ui.actionMenu



-- DELETE MODAL UPDATES


applyDeleteModal : Ui.Modal.Model -> HeaderUi -> HeaderUi
applyDeleteModal newModal headerUi =
    { headerUi | deleteModal = newModal }


applyNewDeleteModal : Container -> Ui.Modal.Model -> Ui.DropdownMenu.Model -> ( Container, Cmd Msg )
applyNewDeleteModal container newModal newMenu =
    ( applyHeaderUi
        (container.headerUi
            |> applyDeleteModal newModal
            |> applyActionMenu newMenu
        )
        container
    , Cmd.none
    )


updateDeleteModalAction : Container -> ModalAction -> ( Container, Cmd Msg )
updateDeleteModalAction container action =
    case action of
        Open ->
            updateDeleteModalOpen container

        Save ->
            updateDeleteModalSave container

        Cancel ->
            updateDeleteModalCancel container


updateDeleteModalOpen : Container -> ( Container, Cmd Msg )
updateDeleteModalOpen container =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.open ui.deleteModal

        newActionMenu =
            Ui.DropdownMenu.close ui.actionMenu
    in
        applyNewDeleteModal container newDeleteModal newActionMenu


updateDeleteModalSave : Container -> ( Container, Cmd Msg )
updateDeleteModalSave container =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.close ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu


updateDeleteModalCancel : Container -> ( Container, Cmd Msg )
updateDeleteModalCancel container =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.close ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu


updateDeleteModalMsg : Container -> Ui.Modal.Msg -> ( Container, Cmd Msg )
updateDeleteModalMsg container msg =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.update msg ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu



-- EDIT FORM UPDATES


updateEditFormMsg : Container -> Form.Msg -> ( Container, Cmd Msg )
updateEditFormMsg container msg =
    case container.headerUi.editForm of
        Just form ->
            let
                ui =
                    container.headerUi

                ( newForm, effect ) =
                    Form.update msg form

                ( newContainer, newEffect ) =
                    ( { container | headerUi = { ui | editForm = Just newForm } }
                    , Cmd.map EditFormMsg effect
                    )
            in
                ( newContainer, newEffect )

        Nothing ->
            ( container, Cmd.none )
