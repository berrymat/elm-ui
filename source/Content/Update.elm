module Content.Update exposing (..)

import Content.Commands exposing (..)
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Messages
import Tree.Update
import RemoteData exposing (..)
import Debug exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Content.Folder


update : Msg -> Content -> ( Content, Cmd Msg )
update message content =
    case message of
        OnFetchFolders nodeId (Ok folders) ->
            ( FoldersContent folders, Cmd.none )

        OnFetchFolders nodeId (Err error) ->
            ( content, Cmd.none )

        OnFetchUsers nodeId (Ok users) ->
            ( UsersContent users, Cmd.none )

        OnFetchUsers nodeId (Err error) ->
            ( content, Cmd.none )

        OnFetchCases nodeId (Ok cases) ->
            ( CasesContent cases, Cmd.none )

        OnFetchCases nodeId (Err error) ->
            ( content, Cmd.none )

        OnFoldersMsg foldersMsg ->
            case content of
                FoldersContent folders ->
                    updateFolders foldersMsg content folders

                _ ->
                    ( content, Cmd.none )


updateFolders : FoldersMsg -> Content -> Folders -> ( Content, Cmd Msg )
updateFolders foldersMsg content folders =
    case foldersMsg of
        OnFetchFiles nodeId (Ok files) ->
            ( FoldersContent { folders | folderId = nodeId, files = files }, Cmd.none )

        OnFetchFiles nodeId (Err error) ->
            ( content, Cmd.none )

        TreeMsg subMsg ->
            let
                ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                    Tree.Update.update subMsg (Success folders.tree)
            in
                updatePathFromTree content folders cmdTree maybePath maybeRoot updatedTree

        SetQuery newQuery ->
            ( FoldersContent { folders | query = newQuery }, Cmd.none )

        SetTableState newState ->
            ( FoldersContent { folders | tableState = newState }, Cmd.none )

        ToggleFile nodeId ->
            let
                newFiles =
                    List.map
                        (\f ->
                            if (f.id == nodeId) then
                                { f | checked = not f.checked }
                            else
                                f
                        )
                        folders.files
            in
                ( FoldersContent { folders | files = newFiles }, Cmd.none )

        -- ACTION MENU
        ActionMenu action ->
            updateActionMenu folders action

        CloseActionMenu ->
            updateCloseActionMenu folders

        NoAction ->
            ( content, Cmd.none )

        -- NEW FOLDER MODAL
        ModalAction NewFolder action ->
            let
                ( newFolders, newCmd ) =
                    case action of
                        Open ->
                            updateNewFolderModalOpen content folders

                        Save ->
                            case folders.newFolderForm of
                                Just form ->
                                    updateNewFolderModalSave content folders form

                                Nothing ->
                                    ( folders, Cmd.none )

                        Cancel ->
                            ( { folders | newFolderModal = Ui.Modal.close folders.newFolderModal }, Cmd.none )
            in
                ( FoldersContent newFolders, newCmd )

        ModalMsg NewFolder modalMsg ->
            let
                newNewFolderModal =
                    Ui.Modal.update modalMsg folders.newFolderModal
            in
                ( FoldersContent { folders | newFolderModal = newNewFolderModal }, Cmd.none )

        NewFolderFormMsg msg ->
            let
                ( newNewFolderModal, effect ) =
                    case folders.newFolderForm of
                        Just form ->
                            let
                                ( newForm, newEffect ) =
                                    Form.update msg form
                            in
                                ( Just newForm, newEffect )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
                ( FoldersContent { folders | newFolderForm = newNewFolderModal }
                , Cmd.map (OnFoldersMsg << NewFolderFormMsg) effect
                )

        FolderInfoPostResponse webdata ->
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

        FolderInfoPutResponse webdata ->
            case webdata of
                NotAsked ->
                    ( content, Cmd.none )

                Loading ->
                    ( content, Cmd.none )

                Failure err ->
                    ( content, Cmd.none )

                Success newFolders ->
                    ( FoldersContent { folders | tree = newFolders.tree }, Cmd.none )

        -- EDIT FOLDER MODAL
        ModalAction EditFolder action ->
            ( content, Cmd.none )

        ModalMsg EditFolder modalMsg ->
            ( content, Cmd.none )

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
                    log "path" (List.head path)

                folderId =
                    case maybeSelected of
                        Just selected ->
                            selected.id

                        Nothing ->
                            updatedTree.id

                cmdFiles =
                    if folderId /= folders.folderId then
                        log "fetchFiles" (fetchFiles folderId)
                    else
                        Cmd.none

                cmdBatch =
                    Cmd.batch
                        [ Cmd.map (OnFoldersMsg << TreeMsg) cmdTree
                        , cmdFiles
                        ]
            in
                ( FoldersContent { folders | tree = updatedTree, path = path }, cmdBatch )

        Nothing ->
            ( FoldersContent folders, Cmd.none )



-- ACTION MENU UPDATES


applyNewActionMenu : Folders -> Ui.DropdownMenu.Model -> ( Content, Cmd Msg )
applyNewActionMenu folders newMenu =
    ( FoldersContent { folders | newFolderActionMenu = newMenu }, Cmd.none )


updateActionMenu : Folders -> Ui.DropdownMenu.Msg -> ( Content, Cmd Msg )
updateActionMenu folders action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action folders.newFolderActionMenu
    in
        applyNewActionMenu folders newActionMenu


updateCloseActionMenu : Folders -> ( Content, Cmd Msg )
updateCloseActionMenu folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.newFolderActionMenu
    in
        applyNewActionMenu folders newActionMenu



-- NEW FOLDER UPDATES


updateNewFolderModalOpen : Content -> Folders -> ( Folders, Cmd Msg )
updateNewFolderModalOpen content folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.newFolderActionMenu

        newFolderForm =
            Content.Folder.newFolderForm folders.folderId
    in
        ( { folders
            | newFolderActionMenu = newActionMenu
            , newFolderModal = Ui.Modal.open folders.newFolderModal
            , newFolderForm = Just newFolderForm
          }
        , Cmd.none
        )


updateNewFolderModalSave : Content -> Folders -> Form.Model Msg -> ( Folders, Cmd Msg )
updateNewFolderModalSave content folders form =
    let
        newNewFolderModal =
            Ui.Modal.close folders.newFolderModal

        emptyFolderInfo =
            Content.Folder.initFolderInfo folders.folderId

        newFolderInfo =
            Content.Folder.updateFolder form emptyFolderInfo

        newEffect =
            postFolderInfo folders.folderId newFolderInfo
    in
        ( { folders | newFolderModal = newNewFolderModal }, newEffect )
