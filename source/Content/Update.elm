module Content.Update exposing (..)

import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Folders.Models
import Folders.Update
import Issues.Update
import Users.Update
import Helpers.Return as Return exposing (..)
import Container.Out exposing (..)


update : Msg -> Content -> ReturnOut Msg OutMsg Content
update message content =
    case ( message, content ) of
        ( FoldersMsg foldersMsg, FoldersContent folders ) ->
            Folders.Update.update foldersMsg folders
                |> wrap
                |> Return.mapBoth FoldersMsg FoldersContent

        ( IssuesMsg issuesMsg, IssuesContent issues ) ->
            Issues.Update.update issuesMsg issues
                |> Return.mapBoth IssuesMsg IssuesContent

        ( UsersMsg usersMsg, UsersContent users ) ->
            Users.Update.update usersMsg users
                |> Return.mapBoth UsersMsg UsersContent

        ( UpdateNode nodeId name, FoldersContent folders ) ->
            updateFoldersNode content nodeId name folders

        x ->
            logStray x content


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


updateFoldersNode : Content -> NodeId -> String -> Folders.Models.Folders -> ReturnOut Msg OutMsg Content
updateFoldersNode content nodeId name folders =
    let
        foldersMsg =
            Folders.Models.UpdateFolder nodeId name

        x =
            Debug.log "updateFoldersNode" ( nodeId, name )
    in
        Folders.Update.update foldersMsg folders
            |> wrap
            |> Return.mapBoth FoldersMsg FoldersContent
