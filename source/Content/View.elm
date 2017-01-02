module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Content.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
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
import Components.Form as Form
import RemoteData exposing (..)


view : WebData Content -> Html Msg
view webdata =
    div [ class "body-content" ]
        (viewWebData webdata viewSuccess viewPendingDefault)


viewSuccess : Content -> List (Html Msg)
viewSuccess content =
    case content of
        FoldersContent folders ->
            contentFolders folders

        UsersContent users ->
            contentUsers users

        CasesContent cases ->
            contentCases cases

        EmptyContent ->
            contentEmpty


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
            [ ( "var-plus", "New", NewFolder )
            , ( "var-record", "Edit", EditFolder )
            , ( "var-arrow-move", "Move", MoveFolder )
            , ( "trash-b", "Delete", DeleteFolder )
            ]

        actionFilter ( _, _, type_ ) =
            case type_ of
                NewFolder ->
                    True

                EditFolder ->
                    True

                MoveFolder ->
                    True

                DeleteFolder ->
                    True

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

        ( saveText, modalType ) =
            case folders.folderEditMethod of
                Just Post ->
                    ( "Create", NewFolder )

                Just Put ->
                    ( "Update", EditFolder )

                Nothing ->
                    ( "", NewFolder )

        folderEditModalViewModel =
            { content = modalContent
            , title = "New Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary saveText (OnFoldersMsg (ModalAction modalType Save))
                    , Ui.Button.secondary "Cancel" (OnFoldersMsg (ModalAction modalType Cancel))
                    ]
                ]
            }
    in
        [ div [ class "body-content-sidebar" ]
            [ div [ class "body-content-sidebar-content" ]
                [ Html.map
                    (OnFoldersMsg << TreeMsg)
                    (Tree.View.view folders.tree)
                ]
            , div [ class "body-content-sidebar-footer" ]
                [ Ui.DropdownMenu.view dropdownViewModel (OnFoldersMsg << ActionMenu) folders.folderActionMenu
                , Ui.Modal.view (OnFoldersMsg << (ModalMsg NewFolder)) folderEditModalViewModel folders.folderEditModal
                ]
            ]
        , div [ class "body-content-content" ]
            [ div [ class "body-content-content-content" ]
                [ contentFiles folders ]
            , div [ class "body-content-content-footer" ]
                []
            ]
        ]


contentFiles : Folders -> Html Msg
contentFiles folders =
    div [ class "file-content" ]
        (viewWebData folders.folder contentFilesSuccess viewPendingDefault)


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
    Table.HtmlDetails []
        [ input [ type_ "checkbox", checked selected, onClick (OnFoldersMsg (ToggleFile nodeId)) ] []
        ]


datetimeColumn : String -> (data -> Float) -> Table.Column data msg
datetimeColumn name toDatetime =
    Table.customColumn
        { name = name
        , viewData = \data -> viewDatetime (toDatetime data)
        , sorter = Table.increasingOrDecreasingBy toDatetime
        }


viewDatetime : Float -> String
viewDatetime datetime =
    let
        date =
            Date.fromTime datetime

        cfg =
            Config.config
    in
        format cfg cfg.format.dateTime date


config : Table.Config File Msg
config =
    Table.customConfig
        { toId = .id
        , toMsg = (OnFoldersMsg << SetTableState)
        , columns =
            [ checkColumn "?" .checked .id
            , Table.stringColumn "Name" .name
            , datetimeColumn "Date/Time" .datetime
            ]
        , customizations = customizations
        }


customizations : Table.Customizations File Msg
customizations =
    { tableAttrs = []
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs
    }


simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name
                    , if selected then
                        darkGrey "↓"
                      else
                        lightGrey "↓"
                    ]

                Table.Reversible Nothing ->
                    [ Html.text name
                    , lightGrey "↕"
                    ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name
                    , darkGrey
                        (if isReversed then
                            "↑"
                         else
                            "↓"
                        )
                    ]
    in
        Html.th [ onClick ] content


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
