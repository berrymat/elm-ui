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
            Folders.Update.update foldersMsg folders
                |> Return.mapBoth FoldersMsg FoldersContent

        _ ->
            singleton content
