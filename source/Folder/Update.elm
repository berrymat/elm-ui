module Folder.Update exposing (..)

import Folder.Models exposing (..)
import Folder.Commands exposing (..)
import Helpers.Models exposing (..)
import Helpers.Ports exposing (..)
import Tree.Models exposing (Tree, Node)
import Tree.Messages
import Tree.Update
import Http exposing (..)
import Table
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Ui.Helpers.Env
import Json.Decode as Decode


type alias ReturnFolder =
    ( Folder, Cmd Msg, Maybe ( String, Request Folder ) )


update : Msg -> Folder -> ReturnFolder
update message folder =
    case message of
        SetQuery newQuery ->
            updateSetQuery folder newQuery

        SetTableState newState ->
            updateSetTableState folder newState

        ToggleFile nodeId ->
            updateToggleFile folder nodeId

        UpdateFolderInfo folderInfo ->
            updateUpdateFolderInfo folder folderInfo

        UpdateMoveTree tree ->
            updateUpdateMoveTree folder tree

        MoveTreeMsg subMsg ->
            updateMoveTreeMsg folder subMsg

        ActionMenu action ->
            updateActionMenu folder action

        CloseActionMenu ->
            updateCloseActionMenu folder

        NoAction ->
            ( folder, Cmd.none, Nothing )

        -- MOVE FILES MODAL
        ModalAction MoveFiles action ->
            updateModalActionMoveFiles folder action

        ModalMsg MoveFiles modalMsg ->
            updateModalMsgMoveFiles folder modalMsg

        -- DELETE FILES MODAL
        ModalAction DeleteFiles action ->
            updateModalActionDeleteFiles folder action

        ModalMsg DeleteFiles modalMsg ->
            updateModalMsgDeleteFiles folder modalMsg

        -- DOWNLOAD FILES MODAL?
        ModalAction DownloadFiles action ->
            updateModalActionDownloadFiles folder action

        ModalMsg DownloadFiles modalMsg ->
            updateModalMsgDownloadFiles folder modalMsg

        -- PORTS
        DownloadResponse response ->
            let
                x =
                    Debug.log "downloadResponse" response
            in
                ( folder, Cmd.none, Nothing )


subscriptions : Folder -> Sub Msg
subscriptions folder =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions folder.filesActionMenu)
    in
        Sub.batch
            [ subActionMenu
            , Helpers.Ports.downloadResponse DownloadResponse
            ]


updateSetQuery : Folder -> String -> ReturnFolder
updateSetQuery folder newQuery =
    ( { folder | query = newQuery }, Cmd.none, Nothing )


updateSetTableState : Folder -> Table.State -> ReturnFolder
updateSetTableState folder newState =
    ( { folder | tableState = newState }, Cmd.none, Nothing )


updateToggleFile : Folder -> NodeId -> ReturnFolder
updateToggleFile folder nodeId =
    let
        newFiles =
            List.map
                (\f ->
                    if (f.id == nodeId) then
                        { f | checked = not f.checked }
                    else
                        f
                )
                folder.files
    in
        ( { folder | files = newFiles }, Cmd.none, Nothing )


updateUpdateFolderInfo : Folder -> FolderInfo -> ReturnFolder
updateUpdateFolderInfo folder folderInfo =
    ( { folder | info = folderInfo }, Cmd.none, Nothing )


updateUpdateMoveTree : Folder -> Tree -> ReturnFolder
updateUpdateMoveTree folder tree =
    ( { folder | moveTree = Just tree }, Cmd.none, Nothing )


updateMoveTreeMsg : Folder -> Tree.Messages.Msg -> ReturnFolder
updateMoveTreeMsg folder subMsg =
    case folder.moveTree of
        Just tree ->
            let
                ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                    Tree.Update.update subMsg (Success tree)
            in
                updateMovePathFromTree folder cmdTree maybePath maybeRoot updatedTree

        Nothing ->
            ( folder, Cmd.none, Nothing )


updateMovePathFromTree : Folder -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> ReturnFolder
updateMovePathFromTree folder cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updateMovePathFromTreeSuccess folder cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( folder, Cmd.none, Nothing )


updateMovePathFromTreeSuccess : Folder -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> ReturnFolder
updateMovePathFromTreeSuccess folder cmdTree maybePath maybeRoot updatedTree =
    case maybePath of
        Just path ->
            ( { folder
                | moveTree = Just updatedTree
              }
            , Cmd.none
            , Nothing
            )

        Nothing ->
            ( folder, Cmd.none, Nothing )



-- ACTION MENU UPDATES


applyNewActionMenu : Folder -> Ui.DropdownMenu.Model -> ReturnFolder
applyNewActionMenu folder newMenu =
    ( { folder | filesActionMenu = newMenu }, Cmd.none, Nothing )


updateActionMenu : Folder -> Ui.DropdownMenu.Msg -> ReturnFolder
updateActionMenu folder action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action folder.filesActionMenu
    in
        applyNewActionMenu folder newActionMenu


updateCloseActionMenu : Folder -> ReturnFolder
updateCloseActionMenu folder =
    let
        newActionMenu =
            Ui.DropdownMenu.close folder.filesActionMenu
    in
        applyNewActionMenu folder newActionMenu



-- MOVE FILES


updateModalActionMoveFiles : Folder -> ModalAction -> ReturnFolder
updateModalActionMoveFiles folder action =
    case action of
        Open ->
            updateMoveFilesModalOpen folder

        Save ->
            updateMoveFilesModalSave folder

        Cancel ->
            ( { folder | filesMoveModal = Ui.Modal.close folder.filesMoveModal }, Cmd.none, Nothing )


updateMoveFilesModalOpen : Folder -> ReturnFolder
updateMoveFilesModalOpen folder =
    let
        newActionMenu =
            Ui.DropdownMenu.close folder.filesActionMenu
    in
        ( { folder
            | filesActionMenu = newActionMenu
            , filesMoveModal = Ui.Modal.open folder.filesMoveModal
          }
        , Cmd.none
        , Nothing
        )


updateMoveFilesModalSave : Folder -> ReturnFolder
updateMoveFilesModalSave folder =
    let
        newFilesMoveModal =
            Ui.Modal.close folder.filesMoveModal

        selectedFiles =
            List.filter (\f -> f.checked) folder.files

        filesUrl tree =
            List.head tree.path
                |> Maybe.map (\node -> node.id)
                |> Maybe.withDefault tree.id
                |> Folder.Commands.filesUrl

        request tree url =
            Http.request
                { method = "PUT"
                , url = url
                , headers = []
                , body = (Http.jsonBody (encodeFiles selectedFiles))
                , expect = (Http.expectJson folderDecoder)
                , timeout = Nothing
                , withCredentials = True
                }

        folderRequest =
            Maybe.map (\tree -> ( filesUrl tree, tree )) folder.moveTree
                |> Maybe.map (\( url, tree ) -> ( url, request tree url ))
    in
        ( { folder
            | filesMoveModal = newFilesMoveModal
          }
        , Cmd.none
        , folderRequest
        )


updateModalMsgMoveFiles : Folder -> Ui.Modal.Msg -> ReturnFolder
updateModalMsgMoveFiles folder modalMsg =
    let
        newFilesMoveModal =
            Ui.Modal.update modalMsg folder.filesMoveModal
    in
        ( { folder | filesMoveModal = newFilesMoveModal }, Cmd.none, Nothing )



-- DELETE FILES


updateModalActionDeleteFiles : Folder -> ModalAction -> ReturnFolder
updateModalActionDeleteFiles folder action =
    case action of
        Open ->
            updateDeleteFilesModalOpen folder

        Save ->
            updateDeleteFilesModalSave folder

        Cancel ->
            ( { folder | filesDeleteModal = Ui.Modal.close folder.filesDeleteModal }, Cmd.none, Nothing )


updateDeleteFilesModalOpen : Folder -> ReturnFolder
updateDeleteFilesModalOpen folder =
    let
        newActionMenu =
            Ui.DropdownMenu.close folder.filesActionMenu
    in
        ( { folder
            | filesActionMenu = newActionMenu
            , filesDeleteModal = Ui.Modal.open folder.filesDeleteModal
          }
        , Cmd.none
        , Nothing
        )


updateDeleteFilesModalSave : Folder -> ReturnFolder
updateDeleteFilesModalSave folder =
    let
        newFilesDeleteModal =
            Ui.Modal.close folder.filesDeleteModal

        url =
            Folder.Commands.filesUrl folder.info.id

        selectedFiles =
            List.filter (\f -> f.checked) folder.files

        request =
            Http.request
                { method = "DELETE"
                , url = url
                , headers = []
                , body = (Http.jsonBody (encodeFiles selectedFiles))
                , expect = (Http.expectJson folderDecoder)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        ( { folder
            | filesDeleteModal = newFilesDeleteModal
          }
        , Cmd.none
        , Just ( url, request )
        )


updateModalMsgDeleteFiles : Folder -> Ui.Modal.Msg -> ReturnFolder
updateModalMsgDeleteFiles folder modalMsg =
    let
        newFilesDeleteModal =
            Ui.Modal.update modalMsg folder.filesDeleteModal
    in
        ( { folder | filesDeleteModal = newFilesDeleteModal }, Cmd.none, Nothing )



-- DOWNLOAD FILES


updateModalActionDownloadFiles : Folder -> ModalAction -> ReturnFolder
updateModalActionDownloadFiles folder action =
    case action of
        Open ->
            updateDownloadFilesModalOpen folder

        _ ->
            ( folder, Cmd.none, Nothing )


updateDownloadFilesModalOpen : Folder -> ReturnFolder
updateDownloadFilesModalOpen folder =
    let
        newActionMenu =
            Ui.DropdownMenu.close folder.filesActionMenu

        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"

        url =
            endpoint ++ folder.info.downloadUrl

        selectedFiles =
            List.filter (\f -> f.checked) folder.files
    in
        ( { folder
            | filesActionMenu = newActionMenu
          }
        , Helpers.Ports.download
            { url = url
            , files = List.map (\f -> f.id) selectedFiles
            }
        , Nothing
        )



-- Dummy method - NOOP


updateModalMsgDownloadFiles : Folder -> Ui.Modal.Msg -> ReturnFolder
updateModalMsgDownloadFiles folder modalMsg =
    ( folder, Cmd.none, Nothing )
