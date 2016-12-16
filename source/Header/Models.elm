module Header.Models exposing (..)

import Container.Messages exposing (Msg)
import Helpers.Models exposing (..)
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form


type alias HeaderUi =
    { actionMenu : Ui.DropdownMenu.Model
    , editModal : Ui.Modal.Model
    , editForm : Maybe (Form.Model Msg)
    , deleteModal : Ui.Modal.Model
    }


type alias HeaderInfo =
    { data : WebData HeaderData
    , ui : HeaderUi
    }


initialHeaderInfo : HeaderInfo
initialHeaderInfo =
    HeaderInfo NotAsked initialHeaderUi


initialHeaderUi : HeaderUi
initialHeaderUi =
    { actionMenu = Ui.DropdownMenu.init
    , editModal = Ui.Modal.init
    , editForm = Nothing
    , deleteModal = Ui.Modal.init
    }


extractId : HeaderData -> NodeId
extractId data =
    case data.header of
        RootHeader root ->
            root.id

        CustomerHeader customer ->
            customer.id

        ClientHeader client ->
            client.id

        SiteHeader site ->
            site.id

        StaffHeader staff ->
            staff.id

        Empty ->
            ""


headerId : HeaderInfo -> NodeId
headerId headerInfo =
    RemoteData.map extractId headerInfo.data
        |> RemoteData.withDefault ""


isHeaderEmpty : HeaderInfo -> Bool
isHeaderEmpty headerInfo =
    (headerInfo.data == NotAsked)


getTabFromType : HeaderInfo -> TabType -> Tab
getTabFromType headerInfo tabType =
    let
        tabs =
            RemoteData.map (\data -> data.tabs) headerInfo.data
                |> RemoteData.withDefault []

        maybeTab =
            tabs
                |> List.filter (\t -> t.tabType == tabType)
                |> List.head

        updatedTab =
            case maybeTab of
                Just tab ->
                    tab

                Nothing ->
                    tabs
                        |> List.head
                        |> Maybe.withDefault (Tab EmptyTab "")
    in
        updatedTab
