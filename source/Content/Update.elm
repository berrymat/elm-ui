module Content.Update exposing (..)

import Content.Commands exposing (..)
import Content.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Messages
import Tree.Update
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Content.Folder


update : Msg -> WebData Content -> ( WebData Content, Cmd Msg )
update message content =
    case message of
        FetchFoldersResponse nodeId folders ->
            ( RemoteData.map FoldersContent folders, Cmd.none )

        FetchUsersResponse nodeId users ->
            ( RemoteData.map UsersContent users, Cmd.none )

        FetchCasesResponse nodeId cases ->
            ( RemoteData.map CasesContent cases, Cmd.none )

        OnFoldersMsg foldersMsg ->
            updateFolders foldersMsg content


updateFolders : FoldersMsg -> WebData Content -> ( WebData Content, Cmd Msg )
updateFolders foldersMsg webdataContent =
    let
        updateFoldersSuccess content =
            case content of
                FoldersContent folders ->
                    updateFoldersContent foldersMsg content folders

                _ ->
                    ( content, Cmd.none )
    in
        RemoteData.update updateFoldersSuccess webdataContent


updateFoldersContent : FoldersMsg -> Content -> Folders -> ( Content, Cmd Msg )
updateFoldersContent foldersMsg content folders =
    case foldersMsg of
        FetchFolderResponse nodeId folder ->
            ( FoldersContent { folders | folderId = nodeId, folder = folder }, Cmd.none )

        TreeMsg subMsg ->
            let
                ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                    Tree.Update.update subMsg (Success folders.tree)
            in
                updatePathFromTree content folders cmdTree maybePath maybeRoot updatedTree

        SetQuery newQuery ->
            let
                newFolder =
                    RemoteData.map (\f -> { f | query = newQuery }) folders.folder
            in
                ( FoldersContent { folders | folder = newFolder }, Cmd.none )

        SetTableState newState ->
            let
                newFolder =
                    RemoteData.map (\f -> { f | tableState = newState }) folders.folder
            in
                ( FoldersContent { folders | folder = newFolder }, Cmd.none )

        ToggleFile nodeId ->
            let
                newFiles folder =
                    List.map
                        (\f ->
                            if (f.id == nodeId) then
                                { f | checked = not f.checked }
                            else
                                f
                        )
                        folder.files

                newFolder =
                    RemoteData.map (\f -> { f | files = newFiles f }) folders.folder
            in
                ( FoldersContent { folders | folder = newFolder }, Cmd.none )

        -- ACTION MENU
        ActionMenu action ->
            updateActionMenu folders action

        CloseActionMenu ->
            updateCloseActionMenu folders

        NoAction ->
            ( content, Cmd.none )

        -- NEW FOLDER MODAL
        ModalAction NewFolder action ->
            updateModalActionFolder folders action Content.Folder.initFolderInfo Post

        ModalMsg NewFolder modalMsg ->
            updateModalMsgFolder folders modalMsg

        -- EDIT FOLDER MODAL
        ModalAction EditFolder action ->
            let
                updateFolder folder =
                    updateModalActionFolder folders action (\id -> folder.info) Put
            in
                RemoteData.map updateFolder folders.folder
                    |> RemoteData.withDefault ( content, Cmd.none )

        ModalMsg EditFolder modalMsg ->
            updateModalMsgFolder folders modalMsg

        FolderFormMsg msg ->
            let
                ( newFolderEditModal, effect ) =
                    maybeUpdate (Form.update msg) folders.folderEditForm
            in
                ( FoldersContent { folders | folderEditForm = newFolderEditModal }
                , Cmd.map (OnFoldersMsg << FolderFormMsg) effect
                )

        FolderInfoSaveResponse webdata ->
            case webdata of
                NotAsked ->
                    ( content, Cmd.none )

                Loading ->
                    ( content, Cmd.none )

                Failure err ->
                    ( content, Cmd.none )

                Success newFolders ->
                    let
                        ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                            Tree.Update.update (Tree.Messages.SelectNode folders.folderId) (Success newFolders.tree)
                    in
                        updatePathFromTree content folders cmdTree maybePath maybeRoot updatedTree

        -- MOVE FOLDER MODAL
        ModalAction MoveFolder action ->
            ( content, Cmd.none )

        ModalMsg MoveFolder modalMsg ->
            ( content, Cmd.none )

        -- DELETE FOLDER MODAL
        ModalAction DeleteFolder action ->
            ( content, Cmd.none )

        ModalMsg DeleteFolder modalMsg ->
            ( content, Cmd.none )


updatePathFromTree : Content -> Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> ( Content, Cmd Msg )
updatePathFromTree content folders cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updatePathFromTreeSuccess content folders cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( content, Cmd.none )


updatePathFromTreeSuccess : Content -> Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> ( Content, Cmd Msg )
updatePathFromTreeSuccess content folders cmdTree maybePath maybeRoot updatedTree =
    case maybePath of
        Just path ->
            let
                maybeSelected =
                    List.head path

                folderId =
                    case maybeSelected of
                        Just selected ->
                            selected.id

                        Nothing ->
                            updatedTree.id

                ( newFolders, cmdFolder ) =
                    if folderId /= folders.folderId then
                        ( { folders | folder = Loading }, fetchFolder folderId )
                    else
                        ( folders, Cmd.none )

                cmdBatch =
                    Cmd.batch
                        [ Cmd.map (OnFoldersMsg << TreeMsg) cmdTree
                        , cmdFolder
                        ]
            in
                ( FoldersContent { newFolders | tree = updatedTree, path = path }, cmdBatch )

        Nothing ->
            ( FoldersContent folders, Cmd.none )



-- ACTION MENU UPDATES


applyNewActionMenu : Folders -> Ui.DropdownMenu.Model -> ( Content, Cmd Msg )
applyNewActionMenu folders newMenu =
    ( FoldersContent { folders | folderActionMenu = newMenu }, Cmd.none )


updateActionMenu : Folders -> Ui.DropdownMenu.Msg -> ( Content, Cmd Msg )
updateActionMenu folders action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action folders.folderActionMenu
    in
        applyNewActionMenu folders newActionMenu


updateCloseActionMenu : Folders -> ( Content, Cmd Msg )
updateCloseActionMenu folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu
    in
        applyNewActionMenu folders newActionMenu



-- NEW FOLDER UPDATES


updateModalActionFolder : Folders -> ModalAction -> (NodeId -> FolderInfo) -> HttpMethod -> ( Content, Cmd Msg )
updateModalActionFolder folders action folderInfo method =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateFolderModalOpen folders folderInfo method

                Save ->
                    case folders.folderEditForm of
                        Just form ->
                            updateFolderModalSave folders folderInfo form method

                        Nothing ->
                            ( folders, Cmd.none )

                Cancel ->
                    ( { folders | folderEditModal = Ui.Modal.close folders.folderEditModal }, Cmd.none )
    in
        ( FoldersContent newFolders, newCmd )


updateFolderModalOpen : Folders -> (NodeId -> FolderInfo) -> HttpMethod -> ( Folders, Cmd Msg )
updateFolderModalOpen folders folderInfo method =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu

        newFolderForm =
            Content.Folder.folderForm (folderInfo folders.folderId)
    in
        ( { folders
            | folderActionMenu = newActionMenu
            , folderEditMethod = Just method
            , folderEditModal = Ui.Modal.open folders.folderEditModal
            , folderEditForm = Just newFolderForm
          }
        , Cmd.none
        )


updateFolderModalSave : Folders -> (NodeId -> FolderInfo) -> Form.Model Msg -> HttpMethod -> ( Folders, Cmd Msg )
updateFolderModalSave folders folderInfo form method =
    let
        newFolderEditModal =
            Ui.Modal.close folders.folderEditModal

        newFolderInfo =
            Content.Folder.updateFolder form (folderInfo folders.folderId)

        newEffect =
            saveFolderInfo folders.folderId newFolderInfo method
    in
        ( { folders | folderEditModal = newFolderEditModal }, newEffect )


updateModalMsgFolder : Folders -> Ui.Modal.Msg -> ( Content, Cmd Msg )
updateModalMsgFolder folders modalMsg =
    let
        newFolderEditModal =
            Ui.Modal.update modalMsg folders.folderEditModal
    in
        ( FoldersContent { folders | folderEditModal = newFolderEditModal }, Cmd.none )
