module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Content.Models exposing (..)
import Folders.Models
import Folders.View
import Users.Models
import Users.View


view : Content -> Html Msg
view content =
    case content of
        FoldersContent folders ->
            viewFolders folders

        UsersContent model ->
            viewUsers model

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


viewUsers : Users.Models.Model -> Html Msg
viewUsers model =
    let
        htmlUsers =
            Users.View.view model
    in
        Html.map UsersMsg htmlUsers


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
