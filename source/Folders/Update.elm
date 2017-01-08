module Folders.Update exposing (..)

import Folders.Models exposing (..)
import Folders.Commands exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Helpers.Progress
import Tree.Messages
import Tree.Models exposing (..)
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
            updateMainTreeMsg folders subMsg

        MoveTreeMsg subMsg ->
            updateMoveTreeMsg folders subMsg

        -- ACTION MENU
        ActionMenu action ->
            updateActionMenu folders action

        CloseActionMenu ->
            updateCloseActionMenu folders

        NoAction ->
            ( folders, Cmd.none )

        -- NEW FOLDER MODAL
        ModalAction NewFolder action ->
            updateModalActionFolder folders action Folder.Commands.initFolderInfo Post

        ModalMsg NewFolder modalMsg ->
            updateModalMsgFolder folders modalMsg

        -- EDIT FOLDER MODAL
        ModalAction EditFolder action ->
            let
                updateFolder folder =
                    updateModalActionFolder folders action (\id -> folder.info) Put
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
            case webdata of
                NotAsked ->
                    ( folders, Cmd.none )

                Loading ->
                    ( folders, Cmd.none )

                Failure err ->
                    ( folders, Cmd.none )

                Success newFolders ->
                    let
                        ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                            Tree.Update.update (Tree.Messages.SelectNode folders.folderId) (Success newFolders.tree)
                    in
                        updateMainPathFromTree folders cmdTree maybePath maybeRoot updatedTree

        -- MOVE FOLDER MODAL
        ModalAction MoveFolder action ->
            Helpers.Progress.map (updateModalActionMoveFolder folders action) folders.folder
                |> Helpers.Progress.withDefault ( folders, Cmd.none )

        ModalMsg MoveFolder modalMsg ->
            updateModalMsgMoveFolder folders modalMsg

        -- DELETE FOLDER MODAL
        ModalAction DeleteFolder action ->
            Helpers.Progress.map (updateModalActionDeleteFolder folders action) folders.folder
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

                x =
                    Debug.log "GetFolderProgress" folder
            in
                ( { folders | folder = newFolder, folderRequest = folderRequest }, Cmd.none )

        FolderMsg folderMsg ->
            updateFolder folderMsg folders

        UploadOpened task ->
            ( folders, Task.perform UploadGetFiles task )

        UploadGetFiles files ->
            let
                ( newFolders, effect ) =
                    upload folders folders.folderId files Folder.Commands.folderDecoder
            in
                ( newFolders, effect )


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


updateMainTreeMsg : Folders -> Tree.Messages.Msg -> Return Msg Folders
updateMainTreeMsg folders subMsg =
    let
        ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
            Tree.Update.update subMsg (Success folders.tree)
    in
        updateMainPathFromTree folders cmdTree maybePath maybeRoot updatedTree


updateMainPathFromTree : Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> Return Msg Folders
updateMainPathFromTree folders cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updateMainPathFromTreeSuccess folders cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( folders, Cmd.none )


updateMainPathFromTreeSuccess : Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> Return Msg Folders
updateMainPathFromTreeSuccess folders cmdTree maybePath maybeRoot updatedTree =
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
                        requestFolder folders folderId
                    else
                        ( folders, Cmd.none )

                cmdBatch =
                    Cmd.batch
                        [ Cmd.map MainTreeMsg cmdTree
                        , cmdFolder
                        ]
            in
                ( { newFolders
                    | tree = updatedTree
                    , moveTree = (createMoveTree updatedTree)
                    , path = path
                  }
                , cmdBatch
                )

        Nothing ->
            ( folders, Cmd.none )


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


updateMoveTreeMsg : Folders -> Tree.Messages.Msg -> Return Msg Folders
updateMoveTreeMsg folders subMsg =
    case folders.moveTree of
        Just tree ->
            let
                ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                    Tree.Update.update subMsg (Success tree)
            in
                updateMovePathFromTree folders cmdTree maybePath maybeRoot updatedTree

        Nothing ->
            ( folders, Cmd.none )


updateMovePathFromTree : Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> Return Msg Folders
updateMovePathFromTree folders cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updateMovePathFromTreeSuccess folders cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( folders, Cmd.none )


updateMovePathFromTreeSuccess : Folders -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> Return Msg Folders
updateMovePathFromTreeSuccess folders cmdTree maybePath maybeRoot updatedTree =
    case maybePath of
        Just path ->
            ( { folders
                | moveTree = Just updatedTree
              }
            , Cmd.none
            )

        Nothing ->
            ( folders, Cmd.none )



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


updateModalActionFolder : Folders -> ModalAction -> (NodeId -> FolderInfo) -> HttpMethod -> Return Msg Folders
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


updateFolderModalSave : Folders -> (NodeId -> FolderInfo) -> Form.Model Msg -> HttpMethod -> Return Msg Folders
updateFolderModalSave folders folderInfo form method =
    let
        newFolderEditModal =
            Ui.Modal.close folders.folderEditModal

        newFolderInfo =
            Folder.Commands.updateFolder form (folderInfo folders.folderId)

        newEffect =
            saveFolderInfo folders.folderId newFolderInfo method

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


updateModalActionMoveFolder : Folders -> ModalAction -> Folder -> Return Msg Folders
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


updateMoveFolderModalSave : Folders -> Folder -> Return Msg Folders
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

        newEffect =
            if newPrefix /= folderInfo.prefix then
                saveFolderInfo folders.folderId newFolderInfo Put
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


updateModalActionDeleteFolder : Folders -> ModalAction -> Folder -> Return Msg Folders
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


updateDeleteFolderModalSave : Folders -> Folder -> Return Msg Folders
updateDeleteFolderModalSave folders folder =
    let
        newFolderDeleteModal =
            Ui.Modal.close folders.folderDeleteModal

        folderInfo =
            folder.info

        newFolderInfo =
            { folderInfo | isDeleted = True }

        newEffect =
            saveFolderInfo folders.folderId newFolderInfo Put
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


upload : Folders -> NodeId -> List Ui.Native.FileManager.File -> Decode.Decoder Folder -> Return Msg Folders
upload folders folderId files decoder =
    let
        part index file =
            Ui.Native.FileManager.toFormData ("file" ++ (toString index)) file

        parts =
            [ (Http.stringPart "id" folderId) ]
                ++ (List.indexedMap part files)

        body =
            Http.multipartBody <| parts

        url =
            (apiUrl ++ "Upload")

        request =
            Http.request
                { method = "POST"
                , url = url
                , headers = []
                , body = body
                , expect = (Http.expectJson decoder)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        ( { folders | folder = Progress.None, folderRequest = Just ( url, request ) }, Cmd.none )
