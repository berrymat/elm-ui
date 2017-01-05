module Folder.Commands exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Folder.Models exposing (..)
import Components.Form as Form
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Table


filesUrl : NodeId -> String
filesUrl nodeId =
    apiUrl ++ "Files/" ++ nodeId


folderDecoder : Decode.Decoder Folder
folderDecoder =
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



-- FORM RELATED?


initFolderInfo : NodeId -> FolderInfo
initFolderInfo folderId =
    FolderInfo folderId "/" "" False False True True True True True True True True True


readableForCustomersName : String
readableForCustomersName =
    "Can this folder be viewed by Customers?"


readableForClientsName : String
readableForClientsName =
    "Can this folder be viewed by Clients?"


readableForStaffName : String
readableForStaffName =
    "Can this folder be viewed by Staff?"


writableForCustomersName : String
writableForCustomersName =
    "Can a Customer upload files to this folder?"


writableForClientsName : String
writableForClientsName =
    "Can a Client upload files to this folder?"


writableForStaffName : String
writableForStaffName =
    "Can Staff upload files to this folder?"


folderForm : FolderInfo -> Form.Model msg
folderForm folderInfo =
    Form.init
        { checkboxes =
            [ ( readableForCustomersName, 11, folderInfo.readableForCustomers )
            , ( readableForClientsName, 12, folderInfo.readableForClients )
            , ( readableForStaffName, 13, folderInfo.readableForStaff )
            , ( writableForCustomersName, 21, folderInfo.writableForCustomers )
            , ( writableForClientsName, 22, folderInfo.writableForClients )
            , ( writableForStaffName, 23, folderInfo.writableForStaff )
            ]
        , inputs =
            [ ( "name", 1, "Name", folderInfo.name )
            ]
        , numberRanges = []
        , textareas = []
        , choosers = []
        , colors = []
        , dates = []
        , titles =
            [ ( "read", 10, "Read Access" )
            , ( "write", 20, "Write Access" )
            ]
        }


newFolderForm : NodeId -> Form.Model msg
newFolderForm folderId =
    folderForm (initFolderInfo folderId)


updateFolder : Form.Model msg -> FolderInfo -> FolderInfo
updateFolder form folderInfo =
    { folderInfo
        | name = Form.valueOfInput "name" folderInfo.name form
        , readableForCustomers = Form.valueOfCheckbox readableForCustomersName folderInfo.readableForCustomers form
        , readableForClients = Form.valueOfCheckbox readableForClientsName folderInfo.readableForClients form
        , readableForStaff = Form.valueOfCheckbox readableForStaffName folderInfo.readableForStaff form
        , writableForCustomers = Form.valueOfCheckbox writableForCustomersName folderInfo.writableForCustomers form
        , writableForClients = Form.valueOfCheckbox writableForClientsName folderInfo.writableForClients form
        , writableForStaff = Form.valueOfCheckbox writableForStaffName folderInfo.writableForStaff form
    }


folderUrl : NodeId -> String
folderUrl nodeId =
    nodeId
