module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, href, id, classList)
import Html.Events exposing (onClick)
import Tree.Messages exposing (..)
import Tree.Models exposing (..)
import RemoteData exposing (..)


view : Tree -> Html Msg
view tree =
    div [ class "flexer" ]
        [ viewTree tree
        ]


viewTree : Tree -> Html Msg
viewTree tree =
    div [ class "k-treeview" ]
        [ ul [ class "k-group", attribute "role" "group", attribute "style" "display: block;" ]
            [ viewRoot tree ]
        ]


viewRoot : Tree -> Html Msg
viewRoot tree =
    let
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
                [ nodeIcon (nodeClasses tree.childrenState) (ToggleNode tree.id)
                , nodeText tree.selected tree.name SelectRoot
                ]
            , ul [ class "k-group", attribute "role" "group", attribute "style" childStyle ]
                (List.map viewNode childNodes)
            ]


viewNode : Node -> Html Msg
viewNode node =
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
            [ nodeView node
            , ul [ class "k-group", attribute "role" "group", attribute "style" childStyle ]
                (viewChildNodes node.childNodes)
            ]


viewChildNodes : WebData ChildNodes -> List (Html Msg)
viewChildNodes childNodes =
    RemoteData.map viewChildren childNodes
        |> RemoteData.withDefault [ div [] [] ]


viewChildren : ChildNodes -> List (Html Msg)
viewChildren (ChildNodes childNodes) =
    List.map viewNode childNodes


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


nodeView : Node -> Html Msg
nodeView node =
    let
        iconMsg =
            case node.childrenState of
                Collapsed ->
                    ToggleNode node.id

                Expanding ->
                    ToggleNode node.id

                Expanded ->
                    ToggleNode node.id

                NoChildren ->
                    NoAction

                RootNode ->
                    OpenNewRoot node.rootType node.id

        iconHtml =
            nodeIcon (nodeClasses node.childrenState) iconMsg

        textHtml =
            nodeText node.selected node.name (SelectNode node.id)
    in
        div [ class "k-mid" ]
            [ iconHtml, textHtml ]


nodeIcon : ( String, String ) -> Msg -> Html Msg
nodeIcon ( iconClass, faClass ) msg =
    span
        [ class iconClass
        , attribute "role" "presentation"
        , onClick msg
        ]
        [ i [ class faClass ] [] ]


nodeText : Bool -> String -> Msg -> Html Msg
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
