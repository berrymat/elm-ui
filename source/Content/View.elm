module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.View
import Table
import Date
import Date.Extra.Config.Config_en_us as Config
import Date.Extra.Format as Format exposing (format)
import Ui.Button
import Ui.Container
import Ui.Modal
import Components.Form as Form


view : Content -> Html Msg
view content =
    case content of
        FoldersContent folders ->
            contentFolders folders

        UsersContent users ->
            contentUsers users

        CasesContent cases ->
            contentCases cases

        EmptyContent ->
            contentEmpty


contentFolders : Folders -> Html Msg
contentFolders folders =
    let
        modalContent =
            case folders.newFolderForm of
                Just form ->
                    [ Form.view (OnFoldersMsg << NewFolderFormMsg) form ]

                Nothing ->
                    [ text "Edit Modal" ]

        newFolderModalViewModel =
            { content = modalContent
            , title = "New Folder"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Create" (OnFoldersMsg (ModalAction NewFolder Save))
                    , Ui.Button.secondary "Cancel" (OnFoldersMsg (ModalAction NewFolder Cancel))
                    ]
                ]
            }
    in
        div [ class "body-content" ]
            [ div [ class "body-content-sidebar" ]
                [ div [ class "body-content-sidebar-content" ]
                    [ Html.map
                        (OnFoldersMsg << TreeMsg)
                        (Tree.View.view folders.tree)
                    ]
                , div [ class "body-content-sidebar-footer" ]
                    [ Ui.Button.view (OnFoldersMsg (ModalAction NewFolder Open)) folders.newFolderButton
                    , Ui.Modal.view (OnFoldersMsg << (ModalMsg NewFolder)) newFolderModalViewModel folders.newFolderModal
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
    let
        lowerQuery =
            String.toLower folders.query

        acceptableFiles =
            List.filter (String.contains lowerQuery << String.toLower << .name) folders.files
    in
        div [ class "file-content" ]
            [ input [ placeholder "Search by Name", onInput (OnFoldersMsg << SetQuery) ] []
            , Table.view config folders.tableState acceptableFiles
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


contentUsers : Users -> Html Msg
contentUsers users =
    div [ class "body-content" ]
        [ div [ class "body-content-content" ]
            [ text "Users" ]
        ]


contentCases : Cases -> Html Msg
contentCases cases =
    div [ class "body-content" ]
        [ div [ class "body-content-content" ]
            [ text "Cases" ]
        ]


contentEmpty : Html Msg
contentEmpty =
    div [ class "body-content" ]
        [ div [ class "body-content-content" ]
            [ text "Empty" ]
        ]
