module Container.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style, attribute, href, id, classList, src)
import Html.Events exposing (onClick)
import Container.Models exposing (..)
import Tree.Models exposing (..)
import Helpers.Models exposing (..)
import Helpers.RemoteData
import Tree.View
import Header.View
import Content.View
import RemoteData exposing (..)


view : AuthToken -> Container -> Html Msg
view token container =
    div [ class "fullview" ]
        [ div [ class "sidebar" ]
            [ div [ class "sidebar-content" ]
                [ viewTree container ]
            , div [ class "sidebar-footer" ]
                [ viewTreeFooter container ]
            ]
        , div [ class "body" ]
            [ viewHeader token container
            , div [ class "body-path" ]
                [ viewPath container
                , viewTabs container
                ]
            , viewContent token container
            ]
        ]


viewTree : Container -> Html Msg
viewTree container =
    let
        htmlTree =
            Helpers.RemoteData.view
                container.tree
                Tree.View.view
                (Helpers.RemoteData.viewPendingDefault "flexer")
    in
        Html.map TreeMsg htmlTree


viewTreeFooter : Container -> Html Msg
viewTreeFooter container =
    text "Footer"


viewHeader : AuthToken -> Container -> Html Msg
viewHeader token container =
    let
        htmlContent =
            Helpers.RemoteData.view
                container.header
                (Header.View.view token)
                (Helpers.RemoteData.viewPendingDefault "body-header")
    in
        Html.map HeaderMsg htmlContent


viewContent : AuthToken -> Container -> Html Msg
viewContent token container =
    let
        htmlContent =
            Helpers.RemoteData.view
                container.content
                (Content.View.view token)
                (Helpers.RemoteData.viewPendingDefault "body-content")
    in
        Html.map ContentMsg htmlContent


clickablePathItem : PathItem -> Html Msg
clickablePathItem item =
    div []
        [ div
            [ class "path-item clickable"
            , onClick (SelectPath item.id)
            ]
            [ text item.name ]
        , i [ class "fa fa-chevron-right" ] []
        ]


lastPathItem : PathItem -> Html Msg
lastPathItem item =
    div [ class "path-item" ]
        [ text item.name ]


viewPath : Container -> Html Msg
viewPath container =
    RemoteData.map (viewPathSuccess container) container.tree
        |> RemoteData.withDefault (div [ class "breadcrumb" ] [])


viewPathSuccess : Container -> Tree -> Html Msg
viewPathSuccess container tree =
    let
        rootItem =
            { id = tree.id, nodeType = tree.nodeType, name = tree.name }

        ( items, last ) =
            case container.path of
                [] ->
                    ( [], rootItem )

                head :: rest ->
                    let
                        pathItems =
                            rest
                                |> List.reverse
                                |> List.map (\n -> { id = n.id, nodeType = n.nodeType, name = n.name })
                    in
                        ( rootItem :: pathItems, { id = head.id, nodeType = head.nodeType, name = head.name } )
    in
        div [ class "breadcrumb" ]
            [ div []
                (List.map clickablePathItem items)
            , lastPathItem last
            ]


viewTabs : Container -> Html Msg
viewTabs container =
    let
        tabs =
            RemoteData.map (\d -> d.tabs) container.header
                |> RemoteData.withDefault []
    in
        div [ class "tabs" ]
            (List.map (tabItem container.tab) tabs)


tabItem : Tab -> Tab -> Html Msg
tabItem selected tab =
    div
        [ classList
            [ ( "tab", True )
            , ( "active", selected.tabType == tab.tabType )
            , ( "clickable", selected.tabType /= tab.tabType )
            ]
        , onClick (SelectTab tab.tabType)
        ]
        [ text tab.name ]
