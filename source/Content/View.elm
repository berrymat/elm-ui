module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Content.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (ChildrenState(..))
import Tree.View
import Table
import Date
import Date.Extra.Config.Config_en_us as Config
import Date.Extra.Format as Format exposing (format)
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Ui.Native.FileManager
import Components.Form as Form
import Helpers.Button
import Helpers.Progress
import Dict


view : Content -> Html Msg
view content =
    div [ class "body-content" ]
        (case content of
            FoldersContent folders ->
                contentFolders folders

            UsersContent users ->
                contentUsers users

            CasesContent cases ->
                contentCases cases

            EmptyContent ->
                contentEmpty
        )


dropdownMenuItem : String -> String -> ModalType -> Html Msg
dropdownMenuItem icon name type_ =
    Ui.DropdownMenu.item [ onClick (OnFoldersMsg (ModalAction type_ Open)) ]
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
                (OnFoldersMsg NoAction)
        , items =
            List.map (\( icon, name, type_ ) -> dropdownMenuItem icon name type_) accessibleActions
        }


contentFolders : Folders -> List (Html Msg)
contentFolders folders =
    let
        dropdownViewModel =
            actionDropdownViewModel folders

        modalContent =
            case folders.folderEditForm of
                Just form ->
                    [ Form.view (OnFoldersMsg << FolderFormMsg) form ]

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
                    [ Ui.Button.primary saveText (OnFoldersMsg (ModalAction modalType Save))
                    , Ui.Button.secondary "Cancel" (OnFoldersMsg (ModalAction modalType Cancel))
                    ]
                ]
            }

        folderMoveModalViewModel =
            { content =
                case folders.moveTree of
                    Just tree ->
                        [ div [ class "padded-modal-content" ]
                            [ Html.map
                                (OnFoldersMsg << MoveTreeMsg)
                                (Tree.View.view tree)
                            ]
                        ]

                    Nothing ->
                        [ text "Can't Move" ]
            , title = "Move Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Move" (OnFoldersMsg (ModalAction MoveFolder Save))
                    , Ui.Button.secondary "Cancel" (OnFoldersMsg (ModalAction MoveFolder Cancel))
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
                    [ Ui.Button.danger "Delete" (OnFoldersMsg (ModalAction DeleteFolder Save))
                    , Ui.Button.secondary "Cancel" (OnFoldersMsg (ModalAction DeleteFolder Cancel))
                    ]
                ]
            }
    in
        [ div [ class "body-content-sidebar" ]
            [ div [ class "body-content-sidebar-content" ]
                [ Html.map
                    (OnFoldersMsg << MainTreeMsg)
                    (Tree.View.view folders.tree)
                ]
            , div [ class "body-content-sidebar-footer" ]
                [ Ui.DropdownMenu.view dropdownViewModel (OnFoldersMsg << ActionMenu) folders.folderActionMenu
                , Ui.Modal.view (OnFoldersMsg << (ModalMsg modalType)) folderEditModalViewModel folders.folderEditModal
                , Ui.Modal.view (OnFoldersMsg << (ModalMsg MoveFolder)) folderMoveModalViewModel folders.folderMoveModal
                , Ui.Modal.view (OnFoldersMsg << (ModalMsg DeleteFolder)) folderDeleteModalViewModel folders.folderDeleteModal
                ]
            ]
        , div [ class "body-content-content" ]
            [ div [ class "body-content-content-content" ]
                [ contentFiles folders ]
            , div [ class "body-content-content-footer" ]
                [ Helpers.Button.primary "Upload" (Ui.Native.FileManager.openMultipleDecoder "*/*" (OnFilesMsg << UploadOpened))
                ]
            ]
        ]


contentFiles : Folders -> Html Msg
contentFiles folders =
    div [ class "file-content" ]
        (Helpers.Progress.view folders.folder contentFilesSuccess viewPendingDefault)


contentFilesSuccess : Folder -> List (Html Msg)
contentFilesSuccess folder =
    let
        lowerQuery =
            String.toLower folder.query

        acceptableFiles =
            List.filter (String.contains lowerQuery << String.toLower << .name) folder.files
    in
        [ input [ placeholder "Search by Name", onInput (OnFoldersMsg << SetQuery) ] []
        , Table.view config folder.tableState acceptableFiles
        ]


checkColumn : String -> (data -> Bool) -> (data -> NodeId) -> Table.Column data Msg
checkColumn name toCheck toId =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewCheck (toCheck data) (toId data)
        , sorter = Table.unsortable
        }


viewCheck : Bool -> NodeId -> Table.HtmlDetails Msg
viewCheck selected nodeId =
    Table.HtmlDetails [ class "checkbox" ]
        [ input [ type_ "checkbox", checked selected, onClick (OnFoldersMsg (ToggleFile nodeId)) ] []
        ]


datetimeColumn : String -> (data -> Float) -> Table.Column data Msg
datetimeColumn name toDatetime =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewDatetime (toDatetime data)
        , sorter = Table.increasingOrDecreasingBy toDatetime
        }


viewDatetime : Float -> Table.HtmlDetails Msg
viewDatetime datetime =
    let
        date =
            Date.fromTime datetime

        cfg =
            Config.config
    in
        Table.HtmlDetails [ class "datetime" ]
            [ text (format cfg cfg.format.dateTime date) ]


config : Table.Config File Msg
config =
    Table.customConfig
        { toId = .id
        , toMsg = (OnFoldersMsg << SetTableState)
        , columns =
            [ checkColumn "select" .checked .id
            , Table.stringColumn "name" .name
            , datetimeColumn "date" .datetime
            ]
        , customizations = customizations
        }


customizations : Table.Customizations File Msg
customizations =
    let
        columnInfo =
            Dict.fromList
                [ ( "select", ( "", "checkbox" ) )
                , ( "name", ( "Name", "name" ) )
                , ( "date", ( "Date/Time", "datetime" ) )
                ]

        simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
        simpleThead headers =
            Table.HtmlDetails [] (List.map simpleTheadHelp headers)

        simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
        simpleTheadHelp ( name, status, onClick ) =
            let
                ( caption, className ) =
                    (Dict.get name columnInfo) |> Maybe.withDefault ( "", "" )

                content =
                    case status of
                        Table.Unsortable ->
                            [ Html.text caption ]

                        Table.Sortable selected ->
                            [ Html.text caption
                            , if selected then
                                darkGrey "↓"
                              else
                                lightGrey "↓"
                            ]

                        Table.Reversible Nothing ->
                            [ Html.text caption
                            , lightGrey "↕"
                            ]

                        Table.Reversible (Just isReversed) ->
                            [ Html.text caption
                            , darkGrey
                                (if isReversed then
                                    "↑"
                                 else
                                    "↓"
                                )
                            ]
            in
                Html.th [ onClick, class className ] content
    in
        { tableAttrs = []
        , caption = Nothing
        , thead = simpleThead
        , tfoot = Nothing
        , tbodyAttrs = []
        , rowAttrs = simpleRowAttrs
        }


darkGrey : String -> Html msg
darkGrey symbol =
    Html.span [ style [ ( "color", "#555" ) ] ] [ Html.text (" " ++ symbol) ]


lightGrey : String -> Html msg
lightGrey symbol =
    Html.span [ style [ ( "color", "#ccc" ) ] ] [ Html.text (" " ++ symbol) ]


simpleRowAttrs : data -> List (Attribute msg)
simpleRowAttrs _ =
    []


contentUsers : Users -> List (Html Msg)
contentUsers users =
    [ div [ class "body-content-content" ]
        [ text "Users" ]
    ]


contentCases : Cases -> List (Html Msg)
contentCases cases =
    [ div [ class "body-content-content" ]
        [ text "Cases" ]
    ]


contentEmpty : List (Html Msg)
contentEmpty =
    [ div [ class "body-content-content" ]
        [ text "Empty" ]
    ]
