module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Content.Models exposing (..)
import Folders.Models
import Folders.View
import Users.Models
import Users.View
import Helpers.Models exposing (..)


view : Content -> AuthToken -> Html Msg
view content token =
    case content of
        FoldersContent folders ->
            viewFolders folders

        UsersContent model ->
            viewUsers model token

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


viewUsers : Users.Models.Model -> AuthToken -> Html Msg
viewUsers model token =
    let
        htmlUsers =
            Users.View.view model token
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
