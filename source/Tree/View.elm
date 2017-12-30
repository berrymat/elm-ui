module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, href, id, classList)
import Html.Events exposing (onClick)
import Tree.Models exposing (..)
import RemoteData exposing (..)


view : Config msg -> Tree -> Html msg
view config tree =
    div [ class "flexer" ]
        [ viewTree config tree
        ]


viewTree : Config msg -> Tree -> Html msg
viewTree config tree =
    div [ class "k-treeview" ]
        [ ul [ class "k-group", attribute "role" "group", attribute "style" "display: block;" ]
            [ viewRoot config tree ]
        ]


viewRoot : Config msg -> Tree -> Html msg
viewRoot config tree =
    let
        (Config cfg) =
            config

        childStyle =
            if tree.childrenState == Expanded then
                "display: block;"
            else
                "display: none; overflow: visible; height: 0px;"

        expandedValue =
            if tree.childrenState == Expanded then
                "true"
            else
                "false"

        nodeStyle =
            "k-in btn regular p0"
                ++ if tree.selected then
                    " k-state-selected"
                   else
                    ""

        (Tree.Models.ChildNodes childNodes) =
            tree.childNodes
    in
        li
            [ class "k-item"
            , attribute "aria-expanded" expandedValue
            , attribute "data-expanded" expandedValue
            , attribute "role" "treeitem"
            ]
            [ div [ class "k-mid" ]
                [ nodeIcon (nodeClasses tree.childrenState) (cfg.treeMsg (ToggleNode tree.id))
                , nodeText tree.selected tree.name (cfg.selectedMsg SelectRoot)
                ]
            , ul [ class "k-group", attribute "role" "group", attribute "style" childStyle ]
                (List.map (viewNode config) childNodes)
            ]


viewNode : Config msg -> Node -> Html msg
viewNode config node =
    let
        childStyle =
            if node.childrenState == Expanded then
                "display: block;"
            else
                "display: none; overflow: visible; height: 0px;"

        expandedValue =
            if node.childrenState == Expanded then
                "true"
            else
                "false"

        -- SelectNewRoot node.nodeType node.id
    in
        li
            [ class "k-item"
            , attribute "aria-expanded" expandedValue
            , attribute "data-expanded" expandedValue
            , attribute "role" "treeitem"
            ]
            [ nodeView config node
            , ul [ class "k-group", attribute "role" "group", attribute "style" childStyle ]
                (viewChildNodes config node.childNodes)
            ]


viewChildNodes : Config msg -> WebData ChildNodes -> List (Html msg)
viewChildNodes config childNodes =
    RemoteData.map (viewChildren config) childNodes
        |> RemoteData.withDefault [ div [] [] ]


viewChildren : Config msg -> ChildNodes -> List (Html msg)
viewChildren config (ChildNodes childNodes) =
    List.map (viewNode config) childNodes


nodeClasses : ChildrenState -> ( String, String )
nodeClasses childrenState =
    case childrenState of
        Collapsed ->
            ( "k-icon k-plus", "fa fa-caret-right" )

        Expanding ->
            ( "k-icon k-minus", "fa fa-spin fa-refresh" )

        Expanded ->
            ( "k-icon k-minus", "fa fa-caret-down" )

        NoChildren ->
            ( "", "" )

        RootNode ->
            ( "k-icon k-minus", "fa fa-angle-double-right" )



-- msg = (ToggleNode node.id)


nodeView : Config msg -> Node -> Html msg
nodeView config node =
    let
        (Config cfg) =
            config

        iconmsg =
            case node.childrenState of
                Collapsed ->
                    cfg.treeMsg (ToggleNode node.id)

                Expanding ->
                    cfg.treeMsg (ToggleNode node.id)

                Expanded ->
                    cfg.treeMsg (ToggleNode node.id)

                NoChildren ->
                    cfg.treeMsg (NoAction)

                RootNode ->
                    cfg.openRootMsg ( node.rootType, node.id ) (OpenNewRoot node.rootType node.id)

        iconHtml =
            nodeIcon (nodeClasses node.childrenState) iconmsg

        textHtml =
            nodeText node.selected node.name (cfg.selectedMsg (SelectNode node.id))
    in
        div [ class "k-mid" ]
            [ iconHtml, textHtml ]


nodeIcon : ( String, String ) -> msg -> Html msg
nodeIcon ( iconClass, faClass ) msg =
    span
        [ class iconClass
        , attribute "role" "presentation"
        , onClick msg
        ]
        [ i [ class faClass ] [] ]


nodeText : Bool -> String -> msg -> Html msg
nodeText selected name msg =
    let
        nodeStyle =
            "k-in btn regular p0"
                ++ if selected then
                    " k-state-selected"
                   else
                    ""
    in
        div
            [ class nodeStyle
            , onClick msg
            ]
            [ text name ]
