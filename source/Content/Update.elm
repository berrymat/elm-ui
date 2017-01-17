module Content.Update exposing (..)

import Content.Models exposing (..)
import Folders.Update
import Issues.Update
import Users.Update
import Return exposing (..)


update : Msg -> Content -> Return Msg Content
update message content =
    case ( message, content ) of
        ( FoldersMsg foldersMsg, FoldersContent folders ) ->
            Folders.Update.update foldersMsg folders
                |> Return.mapBoth FoldersMsg FoldersContent

        ( IssuesMsg issuesMsg, IssuesContent issues ) ->
            Issues.Update.update issuesMsg issues
                |> Return.mapBoth IssuesMsg IssuesContent

        ( UsersMsg usersMsg, UsersContent users ) ->
            Users.Update.update usersMsg users
                |> Return.mapBoth UsersMsg UsersContent

        x ->
            let
                _ =
                    Debug.log "Stray found" x
            in
                singleton content


subscriptions : Content -> Sub Msg
subscriptions content =
    case content of
        FoldersContent folders ->
            Sub.map FoldersMsg (Folders.Update.subscriptions folders)

        IssuesContent issues ->
            Sub.map IssuesMsg (Issues.Update.subscriptions issues)

        UsersContent users ->
            Sub.map UsersMsg (Users.Update.subscriptions users)

        _ ->
            Sub.none
