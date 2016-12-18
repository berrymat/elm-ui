module Container.Commands exposing (..)

import Container.Messages exposing (..)
import Container.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Header.Models exposing (isHeaderEmpty, getTabFromType)
import Tree.Commands
import Header.Commands
import Content.Commands
import Helpers.Helpers exposing (apiUrl)
import Erl
import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import HttpBuilder exposing (..)
import RemoteData exposing (..)
import Components.Form as Form
import Header.Root.View
import Header.Customer.View
import Header.Client.View
import Header.Site.View
import Header.Staff.View


authenticate : String -> String -> NodeType -> NodeId -> Cmd Msg
authenticate username password nodeType nodeId =
    HttpBuilder.get (authenticateUrl username password)
        |> withExpect (Http.expectJson (authenticateDecoder nodeType nodeId))
        |> withCredentials
        |> send (AuthenticateResponse << RemoteData.fromResult)


authenticateUrl : String -> String -> String
authenticateUrl username password =
    let
        erl =
            Erl.parse (apiUrl ++ "Login")
                |> Erl.addQuery "username" username
                |> Erl.addQuery "password" password
    in
        Erl.toString erl


authenticateDecoder : NodeType -> NodeId -> Decode.Decoder AuthResult
authenticateDecoder nodeType nodeId =
    decode (makeAuthResult nodeType nodeId)
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "result" Decode.string


makeAuthResult : NodeType -> NodeId -> String -> String -> String -> AuthResult
makeAuthResult nodeType nodeId resultTypeString resultId result =
    let
        resultType =
            Maybe.withDefault RootType (convertNodeType resultTypeString)
    in
        AuthResult
            (if nodeId == "" then
                resultType
             else
                nodeType
            )
            (if nodeId == "" then
                resultId
             else
                nodeId
            )
            result


fetchIfAuthorized : Container -> AuthResult -> ( Container, Cmd Msg )
fetchIfAuthorized container authResult =
    if authResult.result == "OK" then
        fetchInitialData authResult.nodeType
            authResult.nodeId
            { container | authResult = Success authResult }
    else
        ( container, Cmd.none )


fetchInitialData : NodeType -> NodeId -> Container -> ( Container, Cmd Msg )
fetchInitialData nodeType nodeId container =
    let
        treeCmd =
            Cmd.map TreeMsg (Tree.Commands.fetchRoot nodeId)

        ( newContainer, headerCmd ) =
            Header.Commands.fetchHeader container ( nodeType, nodeId )
    in
        ( newContainer, Cmd.batch [ treeCmd, headerCmd ] )


fetchContent : Container -> HeaderData -> ( Container, Cmd Msg )
fetchContent container headerData =
    let
        ui =
            container.headerUi

        newContainer =
            { container | headerData = Success headerData }

        headerId =
            Header.Models.headerId container.headerData

        updatedHeaderId =
            Header.Models.headerId newContainer.headerData

        updatedTab =
            getTabFromType newContainer.headerData container.tab.tabType

        cmdContent =
            if (headerId /= updatedHeaderId) then
                Content.Commands.fetchContent updatedTab.tabType updatedHeaderId
            else
                Cmd.none

        cmdBatch =
            Cmd.batch
                [ Cmd.map ContentMsg cmdContent
                ]
    in
        ( { newContainer | tab = updatedTab }, cmdBatch )


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
