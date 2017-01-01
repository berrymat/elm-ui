module Content.Folder exposing (..)

import Helpers.Models exposing (..)
import Content.Models exposing (..)
import Components.Form as Form


initFolderInfo : NodeId -> FolderInfo
initFolderInfo folderId =
    FolderInfo folderId "" False True True True True True True


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
