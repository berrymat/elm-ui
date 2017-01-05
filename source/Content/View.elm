module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Content.Models exposing (..)
import Folders.Models
import Folders.View


view : Content -> Html Msg
view content =
    case content of
        FoldersContent folders ->
            viewFolders folders

        UsersContent users ->
            div [ class "body-content" ]
                (contentUsers users)

        CasesContent cases ->
            div [ class "body-content" ]
                (contentCases cases)

        EmptyContent ->
            div [ class "body-content" ]
                (contentEmpty)


viewFolders : Folders.Models.Folders -> Html Msg
viewFolders folders =
    let
        htmlFolders =
            Folders.View.view folders
    in
        Html.map FoldersMsg htmlFolders


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
