module Content.Update exposing (..)

import Content.Models exposing (..)
import Folders.Models
import Folders.Update
import Users.Models
import Users.Update
import Return exposing (..)


update : Msg -> Content -> Return Msg Content
update message content =
    case message of
        FoldersMsg foldersMsg ->
            updateFolders foldersMsg content

        UsersMsg usersMsg ->
            updateUsers usersMsg content


subscriptions : Content -> Sub Msg
subscriptions content =
    case content of
        FoldersContent folders ->
            Sub.map FoldersMsg (Folders.Update.subscriptions folders)

        UsersContent users ->
            Sub.map UsersMsg (Users.Update.subscriptions users)

        _ ->
            Sub.none


updateFolders : Folders.Models.Msg -> Content -> Return Msg Content
updateFolders foldersMsg content =
    case content of
        FoldersContent folders ->
            Folders.Update.update foldersMsg folders
                |> Return.mapBoth FoldersMsg FoldersContent

        _ ->
            singleton content


updateUsers : Users.Models.Msg -> Content -> Return Msg Content
updateUsers usersMsg content =
    case content of
        UsersContent model ->
            Users.Update.update usersMsg model
                |> Return.mapBoth UsersMsg UsersContent

        _ ->
            singleton content
