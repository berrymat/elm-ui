module Content.Commands exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Table
import Debug
import Helpers.Helpers exposing (apiUrl, fetcher)
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Helpers.Helpers exposing (apiUrl, fetcher, poster, putter)


fetchContent : TabType -> NodeId -> Cmd Msg
fetchContent tabType nodeId =
    if nodeId /= "" then
        case tabType of
            FoldersType ->
                let
                    folderCmd =
                        fetchFolders nodeId

                    filesCmd =
                        fetchFiles nodeId
                in
                    Cmd.batch [ folderCmd, filesCmd ]

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
    fetcher (foldersUrl nodeId) foldersDecoder (OnFetchFolders nodeId)


fetchFiles : NodeId -> Cmd Msg
fetchFiles nodeId =
    fetcher (filesUrl nodeId) filesDecoder (OnFoldersMsg << OnFetchFiles nodeId)


fetchUsers : NodeId -> Cmd Msg
fetchUsers nodeId =
    fetcher (usersUrl nodeId) usersDecoder (OnFetchUsers nodeId)


fetchCases : NodeId -> Cmd Msg
fetchCases nodeId =
    fetcher (casesUrl nodeId) casesDecoder (OnFetchCases nodeId)


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
    Decode.map4 createFolders
        (field "id" Decode.string)
        (field "type" Decode.string)
        (field "name" Decode.string)
        (field "children" (Decode.list (Decode.lazy (\_ -> folderDecoder))))


createFolders : NodeId -> String -> String -> List Node -> Folders
createFolders nodeId type_ name children =
    let
        tree =
            createTree
                nodeId
                type_
                name
                children
    in
        Folders
            tree
            Ui.DropdownMenu.init
            Ui.Modal.init
            Nothing
            True
            []
            ""
            Nothing
            []
            (Table.initialSort "Name")
            ""


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


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.map identity
        (field "files" (Decode.list fileDecoder))


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
        , ( "name", Encode.string folderInfo.name )
        , ( "isShared", Encode.bool folderInfo.isShared )
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
        |> required "name" Decode.string
        |> required "isShared" Decode.bool
        |> required "readableForCustomers" Decode.bool
        |> required "readableForClients" Decode.bool
        |> required "readableForStaff" Decode.bool
        |> required "writableForCustomers" Decode.bool
        |> required "writableForClients" Decode.bool
        |> required "writableForStaff" Decode.bool


postFolderInfo : NodeId -> FolderInfo -> Cmd Msg
postFolderInfo folderId folderInfo =
    poster (apiUrl ++ "Folders")
        (encodeFolderInfo folderInfo)
        foldersDecoder
        (OnFoldersMsg << FolderInfoPostResponse << RemoteData.fromResult)


putFolderInfo : NodeId -> FolderInfo -> Cmd Msg
putFolderInfo folderId folderInfo =
    putter (foldersUrl folderId)
        (encodeFolderInfo folderInfo)
        foldersDecoder
        (OnFoldersMsg << FolderInfoPutResponse << RemoteData.fromResult)
