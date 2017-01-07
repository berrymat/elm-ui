module Container.Commands exposing (..)

import Container.Messages exposing (..)
import Container.Models exposing (..)
import Helpers.Models exposing (..)
import Folders.Commands
import Content.Commands exposing (..)
import Helpers.Helpers exposing (apiUrl)
import RemoteData exposing (..)
import Components.Form as Form
import Header.Root.View
import Header.Customer.View
import Header.Client.View
import Header.Site.View
import Header.Staff.View
import Helpers.Helpers exposing (..)


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
    fetcher (Folders.Commands.foldersUrl nodeId)
        Folders.Commands.foldersDecoder
        ((FetchFoldersResponse nodeId) << RemoteData.fromResult)


fetchUsers : NodeId -> Cmd Msg
fetchUsers nodeId =
    fetcher (usersUrl nodeId) usersDecoder ((FetchUsersResponse nodeId) << RemoteData.fromResult)


fetchCases : NodeId -> Cmd Msg
fetchCases nodeId =
    fetcher (casesUrl nodeId) casesDecoder ((FetchCasesResponse nodeId) << RemoteData.fromResult)


initEditForm : Container -> HeaderData -> Maybe (Form.Model Msg)
initEditForm container data =
    case data.header of
        RootHeader root ->
            Just (Header.Root.View.initEditForm root)

        CustomerHeader customer ->
            Just (Header.Customer.View.initEditForm customer)

        ClientHeader client ->
            Just (Header.Client.View.initEditForm client)

        SiteHeader site ->
            Just (Header.Site.View.initEditForm site)

        StaffHeader staff ->
            Just (Header.Staff.View.initEditForm staff)

        Empty ->
            Nothing


updateState : Form.Model Msg -> Container -> HeaderData -> Container
updateState form container data =
    let
        updateHeader header =
            { container | headerData = Success { data | header = header } }
    in
        case data.header of
            RootHeader root ->
                let
                    newRoot =
                        Header.Root.View.updateState form root
                in
                    updateHeader (RootHeader newRoot)

            CustomerHeader customer ->
                let
                    newCustomer =
                        Header.Customer.View.updateState form customer
                in
                    updateHeader (CustomerHeader newCustomer)

            ClientHeader client ->
                let
                    newClient =
                        Header.Client.View.updateState form client
                in
                    updateHeader (ClientHeader newClient)

            SiteHeader site ->
                let
                    newSite =
                        Header.Site.View.updateState form site
                in
                    updateHeader (SiteHeader newSite)

            StaffHeader staff ->
                let
                    newStaff =
                        Header.Staff.View.updateState form staff
                in
                    updateHeader (StaffHeader newStaff)

            Empty ->
                container
