module Content.Commands exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Update exposing (..)
import Tree.Models exposing (..)
import Table
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Helpers.Helpers exposing (..)
import Debug exposing (..)
import Http.Progress as Progress exposing (Progress(..))


fetchContent : TabType -> NodeId -> Cmd Msg
fetchContent tabType nodeId =
    if nodeId /= "" then
        case tabType of
            FoldersType ->
                fetchFolders nodeId

            UsersType ->
                fetchUsers nodeId

            CasesType ->
                fetchCases nodeId

            EmptyTab ->
                Cmd.none
    else
        Cmd.none


fetchFolders : NodeId -> Cmd Msg
fetchFolders nodeId =
    fetcher (foldersUrl nodeId) foldersDecoder ((FetchFoldersResponse nodeId) << RemoteData.fromResult)


fetchUsers : NodeId -> Cmd Msg
fetchUsers nodeId =
    fetcher (usersUrl nodeId) usersDecoder ((FetchUsersResponse nodeId) << RemoteData.fromResult)


fetchCases : NodeId -> Cmd Msg
fetchCases nodeId =
    fetcher (casesUrl nodeId) casesDecoder ((FetchCasesResponse nodeId) << RemoteData.fromResult)


foldersUrl : NodeId -> String
foldersUrl nodeId =
    apiUrl ++ "Folders/" ++ nodeId


filesUrl : NodeId -> String
filesUrl nodeId =
    apiUrl ++ "Files/" ++ nodeId


usersUrl : NodeId -> String
usersUrl nodeId =
    apiUrl ++ "Users/" ++ nodeId


casesUrl : NodeId -> String
casesUrl nodeId =
    apiUrl ++ "Cases/" ++ nodeId



-- DECODERS


foldersDecoder : Decode.Decoder Folders
foldersDecoder =
    decode createFolders
        |> required "tree" foldersTreeDecoder
        |> required "folder" filesDecoder


foldersTreeDecoder : Decode.Decoder Tree
foldersTreeDecoder =
    decode createTree
        |> required "id" Decode.string
        |> required "type" Decode.string
        |> required "name" Decode.string
        |> required "children" (Decode.list (Decode.lazy (\_ -> folderDecoder)))


createFolders : Tree -> Folder -> Folders
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
        (Done folder)
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


usersDecoder : Decode.Decoder Users
usersDecoder =
    Decode.map2 Users
        (field "id" Decode.string)
        (field "name" Decode.string)


casesDecoder : Decode.Decoder Cases
casesDecoder =
    Decode.map2 Cases
        (field "id" Decode.string)
        (field "name" Decode.string)


filesDecoder : Decode.Decoder Folder
filesDecoder =
    decode Folder
        |> required "info" folderInfoDecoder
        |> required "files" (Decode.list fileDecoder)
        |> hardcoded (Table.initialSort "Name")
        |> hardcoded ""


fileDecoder : Decode.Decoder File
fileDecoder =
    decode File
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "datetime" Decode.float
        |> required "writable" Decode.bool
        |> hardcoded False


encodeFolderInfo : FolderInfo -> Encode.Value
encodeFolderInfo folderInfo =
    Encode.object
        [ ( "id", Encode.string folderInfo.id )
        , ( "prefix", Encode.string folderInfo.prefix )
        , ( "name", Encode.string folderInfo.name )
        , ( "isShared", Encode.bool folderInfo.isShared )
        , ( "isDeleted", Encode.bool folderInfo.isDeleted )
        , ( "isWritable", Encode.bool folderInfo.isWritable )
        , ( "isReadable", Encode.bool folderInfo.isReadable )
        , ( "isMovable", Encode.bool folderInfo.isMovable )
        , ( "readableForCustomers", Encode.bool folderInfo.readableForCustomers )
        , ( "readableForClients", Encode.bool folderInfo.readableForClients )
        , ( "readableForStaff", Encode.bool folderInfo.readableForStaff )
        , ( "writableForCustomers", Encode.bool folderInfo.writableForCustomers )
        , ( "writableForClients", Encode.bool folderInfo.writableForClients )
        , ( "writableForStaff", Encode.bool folderInfo.writableForStaff )
        ]


folderInfoDecoder : Decode.Decoder FolderInfo
folderInfoDecoder =
    decode FolderInfo
        |> required "id" Decode.string
        |> required "prefix" Decode.string
        |> required "name" Decode.string
        |> required "isShared" Decode.bool
        |> required "isDeleted" Decode.bool
        |> required "isWritable" Decode.bool
        |> required "isReadable" Decode.bool
        |> required "isMovable" Decode.bool
        |> required "readableForCustomers" Decode.bool
        |> required "readableForClients" Decode.bool
        |> required "readableForStaff" Decode.bool
        |> required "writableForCustomers" Decode.bool
        |> required "writableForClients" Decode.bool
        |> required "writableForStaff" Decode.bool


saveFolderInfo : NodeId -> FolderInfo -> HttpMethod -> Cmd Msg
saveFolderInfo folderId folderInfo method =
    let
        url =
            case method of
                Post ->
                    (apiUrl ++ "Folders")

                Put ->
                    (foldersUrl folderId)
    in
        requester url
            method
            (encodeFolderInfo folderInfo)
            foldersDecoder
            (OnFoldersMsg << FolderInfoSaveResponse << RemoteData.fromResult)
