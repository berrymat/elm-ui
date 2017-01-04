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
import Task exposing (..)
import Ui.Native.FileManager
import Http
import Json.Decode as Decode


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

        OnFilesMsg filesMsg ->
            updateFiles filesMsg content


subscriptions : WebData Content -> Sub Msg
subscriptions content =
    let
        subAction content =
            case content of
                FoldersContent folders ->
                    Sub.map (OnFoldersMsg << ActionMenu) (Ui.DropdownMenu.subscriptions folders.folderActionMenu)

                _ ->
                    Sub.none

        subActionMenu =
            RemoteData.map subAction content
                |> RemoteData.withDefault Sub.none
    in
        Sub.batch
            [ subActionMenu
            ]


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

        MainTreeMsg subMsg ->
            let
                ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                    Tree.Update.update subMsg (Success folders.tree)
            in
                updatePathFromTree content folders cmdTree maybePath maybeRoot updatedTree

        MoveTreeMsg subMsg ->
            case folders.moveTree of
                Just tree ->
                    let
                        ( updatedTree, cmdTree, maybePath, maybeRoot ) =
                            Tree.Update.update subMsg (Success tree)
                    in
                        updateMovePathFromTree content folders cmdTree maybePath maybeRoot updatedTree

                Nothing ->
                    ( content, Cmd.none )

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
            RemoteData.map (updateModalActionMoveFolder folders action) folders.folder
                |> RemoteData.withDefault ( content, Cmd.none )

        ModalMsg MoveFolder modalMsg ->
            updateModalMsgMoveFolder folders modalMsg

        -- DELETE FOLDER MODAL
        ModalAction DeleteFolder action ->
            RemoteData.map (updateModalActionDeleteFolder folders action) folders.folder
                |> RemoteData.withDefault ( content, Cmd.none )

        ModalMsg DeleteFolder modalMsg ->
            updateModalMsgDeleteFolder folders modalMsg


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
                        [ Cmd.map (OnFoldersMsg << MainTreeMsg) cmdTree
                        , cmdFolder
                        ]
            in
                ( FoldersContent
                    { newFolders
                        | tree = updatedTree
                        , moveTree = (createMoveTree updatedTree)
                        , path = path
                    }
                , cmdBatch
                )

        Nothing ->
            ( FoldersContent folders, Cmd.none )


updateMovePathFromTree : Content -> Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> ( Content, Cmd Msg )
updateMovePathFromTree content folders cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updateMovePathFromTreeSuccess content folders cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( content, Cmd.none )


updateMovePathFromTreeSuccess : Content -> Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> ( Content, Cmd Msg )
updateMovePathFromTreeSuccess content folders cmdTree maybePath maybeRoot updatedTree =
    case maybePath of
        Just path ->
            ( FoldersContent
                { folders
                    | moveTree = Just updatedTree
                }
            , Cmd.none
            )

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



-- MOVE FOLDER


updateModalActionMoveFolder : Folders -> ModalAction -> Folder -> ( Content, Cmd Msg )
updateModalActionMoveFolder folders action folder =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateMoveFolderModalOpen folders

                Save ->
                    updateMoveFolderModalSave folders folder

                Cancel ->
                    ( { folders | folderMoveModal = Ui.Modal.close folders.folderMoveModal }, Cmd.none )
    in
        ( FoldersContent newFolders, newCmd )


updateMoveFolderModalOpen : Folders -> ( Folders, Cmd Msg )
updateMoveFolderModalOpen folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu
    in
        ( { folders
            | folderActionMenu = newActionMenu
            , folderMoveModal = Ui.Modal.open folders.folderMoveModal
          }
        , Cmd.none
        )


updateMoveFolderModalSave : Folders -> Folder -> ( Folders, Cmd Msg )
updateMoveFolderModalSave folders folder =
    let
        newFolderMoveModal =
            Ui.Modal.close folders.folderMoveModal

        folderInfo =
            folder.info

        folderPrefix tree =
            "/"
                ++ (List.reverse tree.path
                        |> List.map (\n -> n.name ++ "/")
                        |> String.concat
                   )

        newPrefix =
            Maybe.map folderPrefix folders.moveTree
                |> Maybe.withDefault "/"

        newFolderInfo =
            { folderInfo | prefix = newPrefix }

        newFolder =
            { folder | info = newFolderInfo }

        newEffect =
            if newPrefix /= folderInfo.prefix then
                saveFolderInfo folders.folderId newFolderInfo Put
            else
                Cmd.none
    in
        ( { folders
            | folderMoveModal = newFolderMoveModal
            , folder = Success newFolder
          }
        , newEffect
        )


updateModalMsgMoveFolder : Folders -> Ui.Modal.Msg -> ( Content, Cmd Msg )
updateModalMsgMoveFolder folders modalMsg =
    let
        newFolderMoveModal =
            Ui.Modal.update modalMsg folders.folderMoveModal
    in
        ( FoldersContent { folders | folderMoveModal = newFolderMoveModal }, Cmd.none )



-- DELETE FOLDER


updateModalActionDeleteFolder : Folders -> ModalAction -> Folder -> ( Content, Cmd Msg )
updateModalActionDeleteFolder folders action folder =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateDeleteFolderModalOpen folders

                Save ->
                    updateDeleteFolderModalSave folders folder

                Cancel ->
                    ( { folders | folderDeleteModal = Ui.Modal.close folders.folderDeleteModal }, Cmd.none )
    in
        ( FoldersContent newFolders, newCmd )


updateDeleteFolderModalOpen : Folders -> ( Folders, Cmd Msg )
updateDeleteFolderModalOpen folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu
    in
        ( { folders
            | folderActionMenu = newActionMenu
            , folderDeleteModal = Ui.Modal.open folders.folderDeleteModal
          }
        , Cmd.none
        )


updateDeleteFolderModalSave : Folders -> Folder -> ( Folders, Cmd Msg )
updateDeleteFolderModalSave folders folder =
    let
        newFolderDeleteModal =
            Ui.Modal.close folders.folderDeleteModal

        folderInfo =
            folder.info

        newFolderInfo =
            { folderInfo | isDeleted = True }

        newFolder =
            { folder | info = newFolderInfo }

        newEffect =
            saveFolderInfo folders.folderId newFolderInfo Put
    in
        ( { folders
            | folderDeleteModal = newFolderDeleteModal
            , folder = Success newFolder
          }
        , newEffect
        )


updateModalMsgDeleteFolder : Folders -> Ui.Modal.Msg -> ( Content, Cmd Msg )
updateModalMsgDeleteFolder folders modalMsg =
    let
        newFolderDeleteModal =
            Ui.Modal.update modalMsg folders.folderDeleteModal
    in
        ( FoldersContent { folders | folderDeleteModal = newFolderDeleteModal }, Cmd.none )



-- UPDATE FILES


updateFiles : FilesMsg -> WebData Content -> ( WebData Content, Cmd Msg )
updateFiles filesMsg webdataContent =
    let
        updateFilesSuccess content =
            case content of
                FoldersContent folders ->
                    updateFilesContent filesMsg content folders

                _ ->
                    ( content, Cmd.none )
    in
        RemoteData.update updateFilesSuccess webdataContent


updateFilesContent : FilesMsg -> Content -> Folders -> ( Content, Cmd Msg )
updateFilesContent filesMsg content folders =
    case filesMsg of
        UploadOpened task ->
            ( content, Task.perform (OnFilesMsg << UploadGetFiles) task )

        UploadGetFiles files ->
            let
                x =
                    Debug.log "files" files
            in
                ( content, upload folders.folderId files filesDecoder )

        UploadUploaded response ->
            let
                x =
                    Debug.log "response" response
            in
                ( FoldersContent { folders | folder = response }, Cmd.none )


upload : NodeId -> List Ui.Native.FileManager.File -> Decode.Decoder Folder -> Cmd Msg
upload folderId files decoder =
    let
        part index file =
            Ui.Native.FileManager.toFormData ("file" ++ (toString index)) file

        parts =
            [ (Http.stringPart "id" folderId) ]
                ++ (List.indexedMap part files)

        body =
            Http.multipartBody <| parts

        request =
            Http.request
                { method = "POST"
                , url =
                    (apiUrl ++ "Upload")
                , headers = []
                , body = body
                , expect = (Http.expectJson decoder)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        request
            |> Http.send ((OnFilesMsg << UploadUploaded) << RemoteData.fromResult)



{-
   HttpBuilder.post (apiUrl ++ "Upload")
       |> withMultipartStringBody parts
       |> withExpect (Http.expectJson decoder)
       |> withCredentials
       |> send ((OnFilesMsg << UploadUploaded) << RemoteData.fromResult)
-}
