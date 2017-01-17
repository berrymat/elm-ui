module Content.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Content.Models exposing (..)
import Folders.Models
import Folders.View
import Issues.Models
import Issues.View
import Users.Models
import Users.View
import Helpers.Models exposing (..)


view : AuthToken -> Content -> Html Msg
view token content =
    case content of
        FoldersContent folders ->
            viewFolders token folders

        UsersContent model ->
            viewUsers token model

        IssuesContent model ->
            viewIssues token model

        EmptyContent ->
            div [ class "body-content" ]
                (contentEmpty)


viewFolders : AuthToken -> Folders.Models.Folders -> Html Msg
viewFolders token folders =
    let
        htmlFolders =
            Folders.View.view token folders
    in
        Html.map FoldersMsg htmlFolders


viewUsers : AuthToken -> Users.Models.Model -> Html Msg
viewUsers token model =
    let
        htmlUsers =
            Users.View.view token model
    in
        Html.map UsersMsg htmlUsers


viewIssues : AuthToken -> Issues.Models.Model -> Html Msg
viewIssues token model =
    let
        htmlIssues =
            Issues.View.view token model
    in
        Html.map IssuesMsg htmlIssues


contentEmpty : List (Html Msg)
contentEmpty =
    [ div [ class "body-content-content" ]
        [ text "Empty" ]
    ]
