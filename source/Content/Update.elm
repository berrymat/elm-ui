module Content.Update exposing (..)

import Content.Commands exposing (..)
import Content.Messages exposing (..)
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Messages
import Tree.Update
import RemoteData exposing (..)
import Debug exposing (..)
import Ui.Modal


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

        ModalAction NewFolder action ->
            let
                newNewFolderModal =
                    case action of
                        Open ->
                            Ui.Modal.open folders.newFolderModal

                        Save ->
                            Ui.Modal.close folders.newFolderModal

                        Cancel ->
                            Ui.Modal.close folders.newFolderModal
            in
                ( FoldersContent { folders | newFolderModal = newNewFolderModal }, Cmd.none )

        ModalMsg NewFolder modalMsg ->
            let
                newNewFolderModal =
                    Ui.Modal.update modalMsg folders.newFolderModal
            in
                ( FoldersContent { folders | newFolderModal = newNewFolderModal }, Cmd.none )


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
