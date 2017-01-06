module Folder.Update exposing (..)

import Folder.Models exposing (..)
import Folder.Commands exposing (..)
import Helpers.Models exposing (..)
import Http exposing (..)
import Table
import Return exposing (..)
import Ui.DropdownMenu
import Ui.Modal


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

        ActionMenu action ->
            updateActionMenu folder action

        CloseActionMenu ->
            updateCloseActionMenu folder

        NoAction ->
            ( folder, Cmd.none, Nothing )

        -- DELETE FOLDER MODAL
        ModalAction DeleteFiles action ->
            updateModalActionDeleteFiles folder action

        ModalMsg DeleteFiles modalMsg ->
            updateModalMsgDeleteFiles folder modalMsg

        -- MODALS
        ModalAction modalType modalAction ->
            ( folder, Cmd.none, Nothing )

        ModalMsg modalType modalMsg ->
            ( folder, Cmd.none, Nothing )


subscriptions : Folder -> Sub Msg
subscriptions folder =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions folder.filesActionMenu)
    in
        Sub.batch
            [ subActionMenu
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
