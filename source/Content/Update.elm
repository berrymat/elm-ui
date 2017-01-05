module Content.Update exposing (..)

import Content.Models exposing (..)
import Folders.Models
import Folders.Update
import Return exposing (..)


update : Msg -> Content -> Return Msg Content
update message content =
    case message of
        FoldersMsg foldersMsg ->
            updateFolders foldersMsg content


subscriptions : Content -> Sub Msg
subscriptions content =
    case content of
        FoldersContent folders ->
            Sub.map FoldersMsg (Folders.Update.subscriptions folders)

        _ ->
            Sub.none


updateFolders : Folders.Models.Msg -> Content -> Return Msg Content
updateFolders foldersMsg content =
    case content of
        FoldersContent folders ->
            let
                ( newFolders, foldersCmd ) =
                    Folders.Update.update foldersMsg folders
            in
                ( FoldersContent newFolders, Cmd.map FoldersMsg foldersCmd )

        _ ->
            ( content, Cmd.none )
