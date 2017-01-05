module Folder.Update exposing (..)

import Folder.Models exposing (..)
import Helpers.Models exposing (..)
import Table
import Return exposing (..)


update : Msg -> Folder -> Return Msg Folder
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


updateSetQuery : Folder -> String -> Return Msg Folder
updateSetQuery folder newQuery =
    ( { folder | query = newQuery }, Cmd.none )


updateSetTableState : Folder -> Table.State -> Return Msg Folder
updateSetTableState folder newState =
    ( { folder | tableState = newState }, Cmd.none )


updateToggleFile : Folder -> NodeId -> Return Msg Folder
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
        ( { folder | files = newFiles }, Cmd.none )


updateUpdateFolderInfo : Folder -> FolderInfo -> Return Msg Folder
updateUpdateFolderInfo folder folderInfo =
    ( { folder | info = folderInfo }, Cmd.none )
