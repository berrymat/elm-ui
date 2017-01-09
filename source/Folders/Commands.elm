module Folders.Commands exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Folders.Models exposing (..)
import Folder.Commands
import Folder.Models
import Tree.Models exposing (..)
import Tree.Update exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Ui.DropdownMenu
import Ui.Modal
import RemoteData exposing (..)
import Http.Progress as Progress exposing (Progress(..))


foldersUrl : NodeId -> String
foldersUrl nodeId =
    apiUrl ++ "Folders/" ++ nodeId


foldersDecoder : Decode.Decoder Folders
foldersDecoder =
    decode createFolders
        |> required "tree" foldersTreeDecoder
        |> required "folder" Folder.Commands.folderDecoder


foldersTreeDecoder : Decode.Decoder Tree
foldersTreeDecoder =
    decode createTree
        |> required "id" Decode.string
        |> required "type" Decode.string
        |> required "name" Decode.string
        |> required "children" (Decode.list (Decode.lazy (\_ -> folderDecoder)))


createFolders : Tree -> Folder.Models.Folder -> Folders
createFolders tree folder =
    Folders
        tree
        Ui.DropdownMenu.init
        Nothing
        Ui.Modal.init
        Nothing
        Ui.Modal.init
        Ui.Modal.init
        (createMoveTree tree)
        True
        []
        folder.info.id
        (Done { folder | moveTree = Just tree })
        Nothing


createMoveTree : Tree -> Maybe Tree
createMoveTree tree =
    if tree.selected then
        Nothing
    else
        let
            newTree =
                { id = tree.id
                , nodeType = tree.nodeType
                , name = tree.name
                , selected = False
                , childrenState = tree.childrenState
                , childNodes = (createMoveNodes tree.childNodes)
                , path = []
                }

            maybeSelected =
                List.tail tree.path
                    |> Maybe.andThen List.head
                    |> Maybe.map (\n -> n.id)

            ( moveTree, _ ) =
                selectSuccess maybeSelected newTree
        in
            Just moveTree


createMoveNodes : ChildNodes -> ChildNodes
createMoveNodes (ChildNodes childNodes) =
    let
        newChildNodes node =
            RemoteData.map (\cn -> createMoveNodes cn) node.childNodes

        createNode node =
            { id = node.id
            , nodeType = node.nodeType
            , name = node.name
            , selected = False
            , childrenState = node.childrenState
            , childNodes = (newChildNodes node)
            , rootType = node.rootType
            }
    in
        List.filter (\n -> not n.selected) childNodes
            |> List.map createNode
            |> ChildNodes


folderDecoder : Decode.Decoder Node
folderDecoder =
    Decode.map4 createNode
        (field "id" Decode.string)
        (field "type" Decode.string)
        (field "name" Decode.string)
        (field "children" (Decode.list (Decode.lazy (\_ -> folderDecoder))))


childDecoder : Decode.Decoder Node
childDecoder =
    Decode.map3 createChild
        (field "id" Decode.string)
        (field "type" Decode.string)
        (field "name" Decode.string)


createTree : NodeId -> String -> String -> List Node -> Tree
createTree nodeId type_ name children =
    Tree
        nodeId
        (Maybe.withDefault FolderType (convertNodeType type_))
        name
        True
        Expanded
        (ChildNodes children)
        []


createNode : NodeId -> String -> String -> List Node -> Node
createNode nodeId type_ name children =
    Node
        nodeId
        (Maybe.withDefault FolderType (convertNodeType type_))
        name
        False
        (if (List.length children) == 0 then
            NoChildren
         else
            Expanded
        )
        (Success (ChildNodes children))
        RootType


createChild : NodeId -> String -> String -> Node
createChild nodeId type_ name =
    let
        a =
            Debug.log "nodeId" nodeId

        children =
            []
    in
        Node
            nodeId
            (Maybe.withDefault FolderType (convertNodeType type_))
            name
            False
            (if (List.length children) == 0 then
                NoChildren
             else
                Expanded
            )
            (Success (ChildNodes children))
            RootType


saveFolderInfo : AuthToken -> NodeId -> Folder.Models.FolderInfo -> HttpMethod -> Cmd Msg
saveFolderInfo token folderId folderInfo method =
    requester token
        "Folders"
        folderId
        method
        (Folder.Commands.encodeFolderInfo folderInfo)
        foldersDecoder
        (FolderInfoSaveResponse << RemoteData.fromResult)
