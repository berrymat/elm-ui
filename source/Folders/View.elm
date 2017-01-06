module Folders.View exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Button
import Helpers.Progress
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tree.Models exposing (..)
import Tree.View
import Folders.Models exposing (..)
import Folder.Models
import Folder.View
import Components.Form as Form
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Ui.Native.FileManager


view : Folders -> Html Msg
view folders =
    let
        dropdownViewModel =
            actionDropdownViewModel folders

        modalContent =
            case folders.folderEditForm of
                Just form ->
                    [ Form.view FolderFormMsg form ]

                Nothing ->
                    [ text "Edit Modal" ]

        ( title, saveText, modalType ) =
            case folders.folderEditMethod of
                Just Post ->
                    ( "New Folder", "Create", NewFolder )

                Just Put ->
                    ( "Edit Folder", "Update", EditFolder )

                Nothing ->
                    ( "", "", NewFolder )

        folderEditModalViewModel =
            { content = modalContent
            , title = title
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary saveText (ModalAction modalType Save)
                    , Ui.Button.secondary "Cancel" (ModalAction modalType Cancel)
                    ]
                ]
            }

        folderMoveModalViewModel =
            { content =
                case folders.moveTree of
                    Just tree ->
                        [ div [ class "padded-modal-content" ]
                            [ Html.map
                                MoveTreeMsg
                                (Tree.View.view tree)
                            ]
                        ]

                    Nothing ->
                        [ text "Can't Move" ]
            , title = "Move Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Move" (ModalAction MoveFolder Save)
                    , Ui.Button.secondary "Cancel" (ModalAction MoveFolder Cancel)
                    ]
                ]
            }

        folderName =
            Helpers.Progress.map (\f -> f.info.name) folders.folder
                |> Helpers.Progress.withDefault "No Folder"

        folderDeleteModalViewModel =
            { content =
                [ div [ class "padded-modal-content" ]
                    [ text ("Confirm deletion of folder '" ++ folderName ++ "'?") ]
                ]
            , title = "Delete Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.danger "Delete" (ModalAction DeleteFolder Save)
                    , Ui.Button.secondary "Cancel" (ModalAction DeleteFolder Cancel)
                    ]
                ]
            }
    in
        div [ class "body-content" ]
            [ div [ class "body-content-sidebar" ]
                [ div [ class "body-content-sidebar-content" ]
                    [ Html.map
                        MainTreeMsg
                        (Tree.View.view folders.tree)
                    ]
                , div [ class "body-content-sidebar-footer" ]
                    [ Ui.DropdownMenu.view dropdownViewModel ActionMenu folders.folderActionMenu
                    , Ui.Modal.view (ModalMsg modalType) folderEditModalViewModel folders.folderEditModal
                    , Ui.Modal.view (ModalMsg MoveFolder) folderMoveModalViewModel folders.folderMoveModal
                    , Ui.Modal.view (ModalMsg DeleteFolder) folderDeleteModalViewModel folders.folderDeleteModal
                    ]
                ]
            , div [ class "body-content-content" ]
                [ div [ class "body-content-content-content" ]
                    [ viewFolder folders ]
                , div [ class "body-content-content-footer" ]
                    [ viewFooter folders
                    , div [ class "flexer" ] []
                    , Helpers.Button.primary "Upload" (Ui.Native.FileManager.openMultipleDecoder "*/*" UploadOpened)
                    ]
                ]
            ]


dropdownMenuItem : String -> String -> ModalType -> Html Msg
dropdownMenuItem icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction type_ Open) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


actionDropdownViewModel : Folders -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel folders =
    let
        actions =
            [ ( "plus", "New Folder", NewFolder )
            , ( "record", "Edit Folder", EditFolder )
            , ( "arrow-move", "Move Folder", MoveFolder )
            , ( "trash-b", "Delete Folder", DeleteFolder )
            ]

        actionFilter action =
            Helpers.Progress.map (actionFilterSuccess action) folders.folder
                |> Helpers.Progress.withDefault False

        actionFilterSuccess ( _, _, type_ ) folder =
            case type_ of
                NewFolder ->
                    folder.info.isWritable

                EditFolder ->
                    not folder.info.isShared

                MoveFolder ->
                    folder.info.isMovable

                DeleteFolder ->
                    let
                        childrenState =
                            List.head folders.tree.path
                                |> Maybe.map (\n -> n.childrenState)
                                |> Maybe.withDefault folders.tree.childrenState

                        noChildren =
                            childrenState == NoChildren

                        noFiles =
                            (List.length folder.files) == 0
                    in
                        (not folder.info.isShared) && noChildren && noFiles

        accessibleActions =
            List.filter actionFilter actions
    in
        { element =
            Ui.IconButton.secondary "Folder Actions"
                "chevron-up"
                "right"
                NoAction
        , items =
            List.map (\( icon, name, type_ ) -> dropdownMenuItem icon name type_) accessibleActions
        }


viewFolder : Folders -> Html Msg
viewFolder folders =
    Helpers.Progress.view
        folders.folder
        viewFolderDone
        (Helpers.Progress.viewPendingDefault "file-content")


viewFolderDone : Folder.Models.Folder -> Html Msg
viewFolderDone folder =
    let
        htmlFolder =
            Folder.View.view folder
    in
        Html.map FolderMsg htmlFolder


viewFooter : Folders -> Html Msg
viewFooter folders =
    Helpers.Progress.view
        folders.folder
        viewFooterDone
        (\t -> text "")


viewFooterDone : Folder.Models.Folder -> Html Msg
viewFooterDone folder =
    let
        htmlFolder =
            Folder.View.viewFooter folder
    in
        Html.map FolderMsg htmlFolder
