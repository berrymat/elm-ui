module Container.Update exposing (..)

import Container.Messages exposing (..)
import Container.Commands exposing (..)
import Container.Models exposing (..)
import Content.Models
import Tree.Messages
import Tree.Commands
import Tree.Update
import Tree.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Header.Models exposing (..)
import Header.Commands
import Content.Update
import Navigation
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Return exposing (..)


update : Msg -> Container -> Return Msg Container
update msg container =
    let
        errorCmd =
            case msg of
                FetchFoldersResponse nodeId folders ->
                    Helpers.Helpers.errorCmd folders

                FetchUsersResponse nodeId users ->
                    Helpers.Helpers.errorCmd users

                FetchCasesResponse nodeId cases ->
                    Helpers.Helpers.errorCmd cases

                HeaderResponse isTree newHeaderData ->
                    Helpers.Helpers.errorCmd newHeaderData

                HeaderPutResponse webdata ->
                    Helpers.Helpers.errorCmd webdata

                _ ->
                    Cmd.none

        return =
            updateInner msg container
    in
        (return |> Return.command errorCmd)


updateInner : Msg -> Container -> Return Msg Container
updateInner msg container =
    let
        ui =
            container.headerUi
    in
        case msg of
            GotoHome ->
                updateGotoHome container

            Goto nodeType nodeId ->
                updateGoto container nodeType nodeId

            LoadContainer parentType nodeId childType ->
                updateLoadContainer parentType nodeId childType container

            SelectPath nodeId ->
                let
                    ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
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
                        fetchContent tabType nodeId
                in
                    ( { container | tab = updatedTab, content = Loading }, cmdContent )

            TreeMsg subMsg ->
                let
                    ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                        Tree.Update.update subMsg container.tree
                in
                    updatePathFromTree container cmdTree maybePath maybeRoot updatedTree

            FetchFoldersResponse nodeId folders ->
                ( { container | content = RemoteData.map Content.Models.FoldersContent folders }, Cmd.none )

            FetchUsersResponse nodeId users ->
                ( { container | content = RemoteData.map Content.Models.UsersContent users }, Cmd.none )

            FetchCasesResponse nodeId cases ->
                ( { container | content = RemoteData.map Content.Models.CasesContent cases }, Cmd.none )

            ContentMsg subMsg ->
                RemoteData.update (Content.Update.update subMsg) container.content
                    |> Return.mapBoth ContentMsg (\new -> { container | content = new })

            HeaderResponse isTree newHeaderData ->
                RemoteData.map (updateContent container isTree) newHeaderData
                    |> RemoteData.withDefault (singleton container)

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
    let
        subContent =
            RemoteData.map Content.Update.subscriptions container.content
                |> RemoteData.withDefault Sub.none
    in
        Sub.batch
            [ Sub.map ActionMenu (Ui.DropdownMenu.subscriptions container.headerUi.actionMenu)
            , Sub.map ContentMsg subContent
            ]


updateGotoHome : Container -> Return Msg Container
updateGotoHome container =
    ( container, Navigation.newUrl "#Home" )


updateGoto : Container -> NodeType -> NodeId -> Return Msg Container
updateGoto container nodeType nodeId =
    ( container, goto nodeType nodeId )


goto : NodeType -> NodeId -> Cmd Msg
goto nodeType nodeId =
    let
        path =
            nodeTypeToPath nodeType
    in
        Navigation.newUrl ("#" ++ path ++ "/" ++ nodeId)


updateLoadContainer : NodeType -> NodeId -> NodeType -> Container -> Return Msg Container
updateLoadContainer parentType nodeId childType container =
    let
        x =
            Debug.log "updateLoadContainer" ( parentType, nodeId, childType )

        treeId =
            nodeId ++ "-" ++ (nodeTypeToPath childType)

        treeCmd =
            Cmd.map TreeMsg (Tree.Commands.fetchRoot treeId)

        ( newContainer, headerCmd ) =
            Header.Commands.fetchHeader container ( parentType, nodeId, True )
    in
        ( { newContainer | path = [] }, Cmd.batch [ treeCmd, headerCmd ] )


updatePathFromTree : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> Return Msg Container
updatePathFromTree container cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updatePathFromTreeSuccess container cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( container, Cmd.none )


updatePathFromTreeSuccess : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> Return Msg Container
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


updateContent : Container -> Bool -> HeaderData -> Return Msg Container
updateContent container isTree headerData =
    let
        ui =
            container.headerUi

        childtypes =
            if isTree then
                headerData.childtypes
            else
                container.childtypes

        newContainer =
            { container | headerData = Success headerData, childtypes = childtypes }

        headerId =
            Header.Models.headerId container.headerData

        updatedHeaderId =
            Header.Models.headerId newContainer.headerData

        updatedTab =
            getTabFromType newContainer.headerData container.tab.tabType

        ( updatedContent, cmdContent ) =
            if (headerId /= updatedHeaderId) then
                ( Loading, fetchContent updatedTab.tabType updatedHeaderId )
            else
                ( newContainer.content, Cmd.none )

        cmdBatch =
            Cmd.batch
                [ cmdContent
                ]
    in
        ( { newContainer | tab = updatedTab, content = updatedContent }, cmdBatch )



-- ACTION MENU UPDATES


applyNewActionMenu : Container -> Ui.DropdownMenu.Model -> Return Msg Container
applyNewActionMenu container newMenu =
    let
        headerUi =
            container.headerUi
    in
        ( { container | headerUi = { headerUi | actionMenu = newMenu } }, Cmd.none )


updateActionMenu : Container -> Ui.DropdownMenu.Msg -> Return Msg Container
updateActionMenu container action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action container.headerUi.actionMenu
    in
        applyNewActionMenu container newActionMenu


updateCloseActionMenu : Container -> Return Msg Container
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


applyNewEditModal : Container -> Ui.Modal.Model -> Maybe (Form.Model Msg) -> Ui.DropdownMenu.Model -> Return Msg Container
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


updateEditModalAction : Container -> ModalAction -> Return Msg Container
updateEditModalAction container action =
    case action of
        Open ->
            updateEditModalOpen container

        Save ->
            updateEditModalSave container

        Cancel ->
            updateEditModalCancel container


updateEditModalOpen : Container -> Return Msg Container
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


updateEditModalSave : Container -> Return Msg Container
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


updateEditModalCancel : Container -> Return Msg Container
updateEditModalCancel container =
    let
        ui =
            container.headerUi

        newEditModal =
            Ui.Modal.close ui.editModal
    in
        applyNewEditModal container newEditModal ui.editForm ui.actionMenu


updateEditModalMsg : Container -> Ui.Modal.Msg -> Return Msg Container
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


applyNewDeleteModal : Container -> Ui.Modal.Model -> Ui.DropdownMenu.Model -> Return Msg Container
applyNewDeleteModal container newModal newMenu =
    ( applyHeaderUi
        (container.headerUi
            |> applyDeleteModal newModal
            |> applyActionMenu newMenu
        )
        container
    , Cmd.none
    )


updateDeleteModalAction : Container -> ModalAction -> Return Msg Container
updateDeleteModalAction container action =
    case action of
        Open ->
            updateDeleteModalOpen container

        Save ->
            updateDeleteModalSave container

        Cancel ->
            updateDeleteModalCancel container


updateDeleteModalOpen : Container -> Return Msg Container
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


updateDeleteModalSave : Container -> Return Msg Container
updateDeleteModalSave container =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.close ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu


updateDeleteModalCancel : Container -> Return Msg Container
updateDeleteModalCancel container =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.close ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu


updateDeleteModalMsg : Container -> Ui.Modal.Msg -> Return Msg Container
updateDeleteModalMsg container msg =
    let
        ui =
            container.headerUi

        newDeleteModal =
            Ui.Modal.update msg ui.deleteModal
    in
        applyNewDeleteModal container newDeleteModal ui.actionMenu



-- EDIT FORM UPDATES


updateEditFormMsg : Container -> Form.Msg -> Return Msg Container
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
