module Folders.Update exposing (..)

import Folders.Models exposing (..)
import Folders.Commands exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Helpers.Progress
import Tree.Models exposing (Tree, Node)
import Tree.Update
import Folder.Commands
import Folder.Models exposing (FolderInfo, Folder)
import Folder.Update
import RemoteData exposing (..)
import Ui.DropdownMenu
import Http.Progress as Progress exposing (Progress(..))
import HttpBuilder exposing (..)
import Http
import Ui.Modal
import Components.Form as Form
import Task exposing (Task)
import Ui.Native.FileManager
import Json.Decode as Decode
import Return exposing (..)
import Helpers.Return exposing (ReturnOut)
import Container.Out exposing (..)


update : Msg -> Folders -> Return Msg Folders
update msg folders =
    let
        errorCmd =
            case msg of
                FolderInfoSaveResponse webdata ->
                    Helpers.Helpers.errorCmd webdata

                _ ->
                    Cmd.none

        return =
            updateInner msg folders
    in
        (return |> Return.command errorCmd)


updateInner : Msg -> Folders -> Return Msg Folders
updateInner msg folders =
    case msg of
        MainTreeMsg subMsg ->
            Tree.Update.update subMsg (Success folders.tree)
                |> updateMainPathFromTree folders

        MainSelectedNodeMsg subMsg ->
            Tree.Update.update subMsg (Success folders.tree)
                |> updateMainSelectedNodeFromTree folders

        MainOpenRootMsg ( _, _ ) subMsg ->
            Tree.Update.update subMsg (Success folders.tree)
                |> updateMainPathFromTree folders

        MoveTreeMsg subMsg ->
            updateMoveTreeMsg folders subMsg

        MoveSelectedNodeMsg subMsg ->
            updateMoveTreeMsg folders subMsg

        MoveOpenRootMsg ( _, _ ) subMsg ->
            updateMoveTreeMsg folders subMsg

        -- ACTION MENU
        ActionMenu action ->
            updateActionMenu folders action

        CloseActionMenu ->
            updateCloseActionMenu folders

        NoAction ->
            ( folders, Cmd.none )

        -- NEW FOLDER MODAL
        ModalAction token NewFolder action ->
            updateModalActionFolder token folders action Folder.Commands.initFolderInfo Post

        ModalMsg NewFolder modalMsg ->
            updateModalMsgFolder folders modalMsg

        -- EDIT FOLDER MODAL
        ModalAction token EditFolder action ->
            let
                updateFolder folder =
                    updateModalActionFolder token folders action (\id -> folder.info) Put
            in
                Helpers.Progress.map updateFolder folders.folder
                    |> Helpers.Progress.withDefault ( folders, Cmd.none )

        ModalMsg EditFolder modalMsg ->
            updateModalMsgFolder folders modalMsg

        FolderFormMsg msg ->
            let
                ( newFolderEditModal, effect ) =
                    maybeUpdate (Form.update msg) folders.folderEditForm
            in
                ( { folders | folderEditForm = newFolderEditModal }
                , Cmd.map FolderFormMsg effect
                )

        FolderInfoSaveResponse webdata ->
            let
                handleNewFolders newFolders =
                    Tree.Update.update (Tree.Models.SelectNode folders.folderId) (Success newFolders.tree)
                        |> updateMainPathFromTree folders
            in
                handleWebDataResponse folders webdata "Folders updated" handleNewFolders

        -- MOVE FOLDER MODAL
        ModalAction token MoveFolder action ->
            Helpers.Progress.map (updateModalActionMoveFolder token folders action) folders.folder
                |> Helpers.Progress.withDefault ( folders, Cmd.none )

        ModalMsg MoveFolder modalMsg ->
            updateModalMsgMoveFolder folders modalMsg

        -- DELETE FOLDER MODAL
        ModalAction token DeleteFolder action ->
            Helpers.Progress.map (updateModalActionDeleteFolder token folders action) folders.folder
                |> Helpers.Progress.withDefault ( folders, Cmd.none )

        ModalMsg DeleteFolder modalMsg ->
            updateModalMsgDeleteFolder folders modalMsg

        -- Folder request stuff
        GetFolder request ->
            ( { folders | folderRequest = Just request }, Cmd.none )

        GetFolderProgress folder ->
            let
                ( folderRequest, newFolder ) =
                    case folder of
                        Done done ->
                            ( Nothing, Done { done | moveTree = Just folders.tree } )

                        _ ->
                            ( folders.folderRequest, folder )
            in
                ( { folders | folder = newFolder, folderRequest = folderRequest }, Cmd.none )

        FolderMsg folderMsg ->
            updateFolder folderMsg folders

        UploadOpened token task ->
            ( folders, Task.perform (UploadGetFiles token) task )

        UploadGetFiles token files ->
            let
                ( newFolders, effect ) =
                    upload token folders folders.folderId files Folder.Commands.folderDecoder
            in
                ( newFolders, effect )

        UpdateFolder nodeId nodeName ->
            Tree.Update.update (Tree.Models.UpdateNode nodeId nodeName) (Success folders.tree)
                |> updateMainPathFromTree folders


subscriptions : Folders -> Sub Msg
subscriptions folders =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions folders.folderActionMenu)

        subProgress =
            case folders.folderRequest of
                Just ( tag, request ) ->
                    request
                        |> Progress.track tag GetFolderProgress

                Nothing ->
                    Sub.none

        subFolder =
            Helpers.Progress.subscriptions Folder.Update.subscriptions folders.folder
                |> Sub.map FolderMsg
    in
        Sub.batch
            [ subActionMenu
            , subProgress
            , subFolder
            ]


updateMainSelectedNodeFromTree : Folders -> ReturnOut Tree.Models.Msg OutMsg (WebData Tree) -> Return Msg Folders
updateMainSelectedNodeFromTree folders return =
    let
        selectPath ( folders, cmd ) tree =
            let
                maybeSelected =
                    List.head tree.path

                folderId =
                    case maybeSelected of
                        Just selected ->
                            selected.id

                        Nothing ->
                            tree.id

                ( newFolders, cmdFolder ) =
                    if folderId /= folders.folderId then
                        requestFolder folders folderId
                    else
                        ( folders, Cmd.none )

                cmdBatch =
                    Cmd.batch
                        [ cmdFolder
                        , cmd
                        ]
            in
                ( { newFolders
                    | tree = tree
                    , moveTree = (createMoveTree tree)
                    , path = tree.path
                  }
                , cmdBatch
                )

        ( newFolders, cmdTree ) =
            updateMainPathFromTree folders return
    in
        selectPath ( newFolders, cmdTree ) newFolders.tree


updateMainPathFromTree : Folders -> ReturnOut Tree.Models.Msg OutMsg (WebData Tree) -> Return Msg Folders
updateMainPathFromTree folders ( ( updatedTree, cmdTree ), outmsgs ) =
    RemoteData.map (updateMainPathFromTreeSuccess folders cmdTree) updatedTree
        |> RemoteData.withDefault ( folders, Cmd.none )


updateMainPathFromTreeSuccess : Folders -> Cmd Tree.Models.Msg -> Tree -> Return Msg Folders
updateMainPathFromTreeSuccess folders cmdTree updatedTree =
    ( { folders | tree = updatedTree }, Cmd.map MainTreeMsg cmdTree )


requestFolder : Folders -> NodeId -> Return Msg Folders
requestFolder folders folderId =
    let
        url =
            (Folder.Commands.filesUrl folderId)

        request =
            HttpBuilder.get url
                |> withExpect (Http.expectJson Folder.Commands.folderDecoder)
                |> withCredentials
                |> toRequest
    in
        ( { folders | folderId = folderId, folder = Progress.None, folderRequest = Just ( url, request ) }, Cmd.none )


updateMoveTreeMsg : Folders -> Tree.Models.Msg -> Return Msg Folders
updateMoveTreeMsg folders subMsg =
    case folders.moveTree of
        Just tree ->
            let
                ( ( updatedTree, cmdTree ), outmsgs ) =
                    Tree.Update.update subMsg (Success tree)
            in
                updateMovePathFromTree folders cmdTree outmsgs updatedTree

        Nothing ->
            ( folders, Cmd.none )


updateMovePathFromTree : Folders -> Cmd Tree.Models.Msg -> List OutMsg -> WebData Tree -> Return Msg Folders
updateMovePathFromTree folders cmdTree outmsgs updatedTree =
    RemoteData.map (updateMovePathFromTreeSuccess folders cmdTree outmsgs) updatedTree
        |> RemoteData.withDefault ( folders, Cmd.none )


updateMovePathFromTreeSuccess : Folders -> Cmd Tree.Models.Msg -> List OutMsg -> Tree -> Return Msg Folders
updateMovePathFromTreeSuccess folders cmdTree outmsgs updatedTree =
    let
        applyOut outmsg newFolders =
            case outmsg of
                OutTreePath _ ->
                    { newFolders | moveTree = Just updatedTree }

                _ ->
                    newFolders
    in
        List.foldl applyOut folders outmsgs |> singleton



-- ACTION MENU UPDATES


applyNewActionMenu : Folders -> Ui.DropdownMenu.Model -> Return Msg Folders
applyNewActionMenu folders newMenu =
    ( { folders | folderActionMenu = newMenu }, Cmd.none )


updateActionMenu : Folders -> Ui.DropdownMenu.Msg -> Return Msg Folders
updateActionMenu folders action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action folders.folderActionMenu
    in
        applyNewActionMenu folders newActionMenu


updateCloseActionMenu : Folders -> Return Msg Folders
updateCloseActionMenu folders =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu
    in
        applyNewActionMenu folders newActionMenu



-- NEW FOLDER UPDATES


updateModalActionFolder : AuthToken -> Folders -> ModalAction -> (NodeId -> FolderInfo) -> HttpMethod -> Return Msg Folders
updateModalActionFolder token folders action folderInfo method =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateFolderModalOpen folders folderInfo method

                Save ->
                    case folders.folderEditForm of
                        Just form ->
                            updateFolderModalSave token folders folderInfo form method

                        Nothing ->
                            ( folders, Cmd.none )

                Cancel ->
                    ( { folders | folderEditModal = Ui.Modal.close folders.folderEditModal }, Cmd.none )
    in
        ( newFolders, newCmd )


updateFolderModalOpen : Folders -> (NodeId -> FolderInfo) -> HttpMethod -> Return Msg Folders
updateFolderModalOpen folders folderInfo method =
    let
        newActionMenu =
            Ui.DropdownMenu.close folders.folderActionMenu

        newFolderForm =
            Folder.Commands.folderForm (folderInfo folders.folderId)
    in
        ( { folders
            | folderActionMenu = newActionMenu
            , folderEditMethod = Just method
            , folderEditModal = Ui.Modal.open folders.folderEditModal
            , folderEditForm = Just newFolderForm
          }
        , Cmd.none
        )


updateFolderModalSave : AuthToken -> Folders -> (NodeId -> FolderInfo) -> Form.Model -> HttpMethod -> Return Msg Folders
updateFolderModalSave token folders folderInfo form method =
    let
        newFolderEditModal =
            Ui.Modal.close folders.folderEditModal

        newFolderInfo =
            Folder.Commands.updateFolder form (folderInfo folders.folderId)

        newEffect =
            saveFolderInfo token folders.folderId newFolderInfo method

        ( newFolders, effect ) =
            updateFolder (Folder.Models.UpdateFolderInfo newFolderInfo) folders
                |> command newEffect
    in
        ( { newFolders | folderEditModal = newFolderEditModal }, effect )


updateModalMsgFolder : Folders -> Ui.Modal.Msg -> Return Msg Folders
updateModalMsgFolder folders modalMsg =
    let
        newFolderEditModal =
            Ui.Modal.update modalMsg folders.folderEditModal
    in
        ( { folders | folderEditModal = newFolderEditModal }, Cmd.none )



-- MOVE FOLDER


updateModalActionMoveFolder : AuthToken -> Folders -> ModalAction -> Folder -> Return Msg Folders
updateModalActionMoveFolder token folders action folder =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateMoveFolderModalOpen folders

                Save ->
                    updateMoveFolderModalSave token folders folder

                Cancel ->
                    ( { folders | folderMoveModal = Ui.Modal.close folders.folderMoveModal }, Cmd.none )
    in
        ( newFolders, newCmd )


updateMoveFolderModalOpen : Folders -> Return Msg Folders
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


updateMoveFolderModalSave : AuthToken -> Folders -> Folder -> Return Msg Folders
updateMoveFolderModalSave token folders folder =
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

        newEffect =
            if newPrefix /= folderInfo.prefix then
                saveFolderInfo token folders.folderId newFolderInfo Put
            else
                Cmd.none

        ( newFolders, effect ) =
            updateFolder (Folder.Models.UpdateFolderInfo newFolderInfo) folders
                |> command newEffect
    in
        ( { newFolders
            | folderMoveModal = newFolderMoveModal
          }
        , effect
        )


updateModalMsgMoveFolder : Folders -> Ui.Modal.Msg -> Return Msg Folders
updateModalMsgMoveFolder folders modalMsg =
    let
        newFolderMoveModal =
            Ui.Modal.update modalMsg folders.folderMoveModal
    in
        ( { folders | folderMoveModal = newFolderMoveModal }, Cmd.none )



-- DELETE FOLDER


updateModalActionDeleteFolder : AuthToken -> Folders -> ModalAction -> Folder -> Return Msg Folders
updateModalActionDeleteFolder token folders action folder =
    let
        ( newFolders, newCmd ) =
            case action of
                Open ->
                    updateDeleteFolderModalOpen folders

                Save ->
                    updateDeleteFolderModalSave token folders folder

                Cancel ->
                    ( { folders | folderDeleteModal = Ui.Modal.close folders.folderDeleteModal }, Cmd.none )
    in
        ( newFolders, newCmd )


updateDeleteFolderModalOpen : Folders -> Return Msg Folders
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


updateDeleteFolderModalSave : AuthToken -> Folders -> Folder -> Return Msg Folders
updateDeleteFolderModalSave token folders folder =
    let
        newFolderDeleteModal =
            Ui.Modal.close folders.folderDeleteModal

        folderInfo =
            folder.info

        newFolderInfo =
            { folderInfo | isDeleted = True }

        newEffect =
            saveFolderInfo token folders.folderId newFolderInfo Put
    in
        ( { folders
            | folderDeleteModal = newFolderDeleteModal
            , folder = Progress.None
          }
        , newEffect
        )


updateModalMsgDeleteFolder : Folders -> Ui.Modal.Msg -> Return Msg Folders
updateModalMsgDeleteFolder folders modalMsg =
    let
        newFolderDeleteModal =
            Ui.Modal.update modalMsg folders.folderDeleteModal
    in
        ( { folders | folderDeleteModal = newFolderDeleteModal }, Cmd.none )


updateFolder : Folder.Models.Msg -> Folders -> Return Msg Folders
updateFolder folderMsg folders =
    case folders.folder of
        Done data ->
            let
                ( newFolder, folderCmd, folderRequest ) =
                    Folder.Update.update folderMsg data

                progressFolder =
                    case folderRequest of
                        Just _ ->
                            Progress.None

                        Nothing ->
                            Done newFolder
            in
                ( { folders
                    | folder = progressFolder
                    , folderRequest = folderRequest
                  }
                , Cmd.map FolderMsg folderCmd
                )

        _ ->
            ( folders, Cmd.none )


upload : AuthToken -> Folders -> NodeId -> List Ui.Native.FileManager.File -> Decode.Decoder Folder -> Return Msg Folders
upload token folders folderId files decoder =
    let
        part index file =
            Ui.Native.FileManager.toFormData ("file" ++ (toString index)) file

        parts =
            [ (Http.stringPart "id" folderId) ]
                ++ (List.indexedMap part files)

        request =
            Helpers.Helpers.multipartRequest token "Upload" folderId Post parts decoder
    in
        ( { folders | folder = Progress.None, folderRequest = Just request }, Cmd.none )
