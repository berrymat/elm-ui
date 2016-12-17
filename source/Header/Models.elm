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


headerId : WebData HeaderData -> NodeId
headerId data =
    RemoteData.map extractId data
        |> RemoteData.withDefault ""


isHeaderEmpty : WebData HeaderData -> Bool
isHeaderEmpty data =
    (data == NotAsked)


getTabFromType : WebData HeaderData -> TabType -> Tab
getTabFromType data tabType =
    let
        tabs =
            RemoteData.map (\data -> data.tabs) data
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
