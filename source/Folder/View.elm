module Folder.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers.Models exposing (..)
import Helpers.Table exposing (..)
import Folder.Models exposing (..)
import Tree.Models
import Tree.View
import Table
import Dict
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Ui.Helpers.Env
import Json.Decode as Decode


moveConfig : Tree.Models.Config Msg
moveConfig =
    Tree.Models.config
        { treeMsg = MoveTreeMsg
        , selectedMsg = MoveSelectedNodeMsg
        , openRootMsg = MoveOpenRootMsg
        }


view : AuthToken -> Folder -> Html Msg
view token folder =
    let
        lowerQuery =
            String.toLower folder.query

        acceptableFiles =
            List.filter (String.contains lowerQuery << String.toLower << .name) folder.files
    in
        div [ class "file-content" ]
            [ input [ placeholder "Search by Name", onInput SetQuery ] []
            , Table.view config folder.tableState acceptableFiles
            ]


selectedCount : Folder -> Int
selectedCount folder =
    List.length (selectedFiles folder)


selectedFiles : Folder -> List File
selectedFiles folder =
    List.filter (\f -> f.checked) folder.files


viewFooter : AuthToken -> Folder -> Html Msg
viewFooter token folder =
    let
        files =
            selectedFiles folder

        prompt =
            case files of
                [] ->
                    ""

                [ file ] ->
                    "Confirm deletion of file '" ++ file.name ++ "'?"

                _ ->
                    "Confirm deletion of " ++ (toString (List.length files)) ++ " files?"

        filesMoveModalViewModel =
            { content =
                case folder.moveTree of
                    Just tree ->
                        [ div [ class "padded-modal-content" ]
                            [ Tree.View.view moveConfig tree
                            ]
                        ]

                    Nothing ->
                        [ text "Can't Move" ]
            , title = "Move Files"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Move" (ModalAction token MoveFiles Save)
                    , Ui.Button.secondary "Cancel" (ModalAction token MoveFiles Cancel)
                    ]
                ]
            }

        filesDeleteModalViewModel =
            { content =
                [ div [ class "padded-modal-content" ]
                    [ text prompt ]
                ]
            , title = "Delete Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.danger "Delete" (ModalAction token DeleteFiles Save)
                    , Ui.Button.secondary "Cancel" (ModalAction token DeleteFiles Cancel)
                    ]
                ]
            }
    in
        if (selectedCount folder) > 0 then
            div []
                [ Ui.DropdownMenu.view (actionDropdownViewModel token folder) ActionMenu folder.filesActionMenu
                , Ui.Modal.view (ModalMsg MoveFiles) filesMoveModalViewModel folder.filesMoveModal
                , Ui.Modal.view (ModalMsg DeleteFiles) filesDeleteModalViewModel folder.filesDeleteModal
                ]
        else
            text ""


nameColumn : String -> (data -> String) -> (data -> String) -> Table.Column data Msg
nameColumn name toName toUrl =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewName (toName data) (toUrl data)
        , sorter = Table.increasingOrDecreasingBy toName
        }


viewName : String -> String -> Table.HtmlDetails Msg
viewName name url =
    let
        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"

        fullurl =
            endpoint ++ url
    in
        Table.HtmlDetails [ class "name" ]
            [ a [ href fullurl, target "_blank" ] [ text name ]
            ]


config : Table.Config File Msg
config =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ checkColumn "select" .checked .id ToggleFile
            , nameColumn "name" .name .url
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


dropdownMenuItem : AuthToken -> String -> String -> ModalType -> Html Msg
dropdownMenuItem token icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction token type_ Open) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


actionDropdownViewModel : AuthToken -> Folder -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel token folder =
    let
        actions =
            [ ( "arrow-move", "Move", MoveFiles )
            , ( "trash-b", "Delete", DeleteFiles )
            , ( "ios-cloud-download", "Download", DownloadFiles )
            ]

        actionFilter ( _, _, type_ ) =
            case type_ of
                MoveFiles ->
                    folder.info.isWritable

                DeleteFiles ->
                    folder.info.isWritable

                DownloadFiles ->
                    True

        suffix =
            " File"
                ++ (if (selectedCount folder) > 1 then
                        "s"
                    else
                        ""
                   )

        accessibleActions =
            List.filter actionFilter actions
    in
        { element =
            Ui.IconButton.secondary "File Actions"
                "chevron-up"
                "right"
                NoAction
        , items =
            List.map (\( icon, name, type_ ) -> dropdownMenuItem token icon (name ++ suffix) type_) accessibleActions
        }
