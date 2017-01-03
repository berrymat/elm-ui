module Container.Commands exposing (..)

import Container.Messages exposing (..)
import Container.Models exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Header.Models exposing (isHeaderEmpty, getTabFromType)
import Tree.Commands
import Header.Commands exposing (..)
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
import Helpers.Helpers exposing (..)


authenticate : String -> String -> Cmd Msg
authenticate username password =
    HttpBuilder.get (authenticateUrl username password)
        |> withExpect (Http.expectJson authenticateDecoder)
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


authenticateDecoder : Decode.Decoder AuthResult
authenticateDecoder =
    decode makeAuthResult
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "result" Decode.string
        |> required "childtypes" (Decode.list entityDecoder)


makeAuthResult : String -> String -> String -> List Entity -> AuthResult
makeAuthResult resultTypeString resultId result childtypes =
    let
        resultType =
            Maybe.withDefault RootType (convertNodeType resultTypeString)
    in
        AuthResult
            resultType
            resultId
            result
            childtypes


maybeAuthResultTypes : AuthResult -> Maybe ( NodeType, NodeId, NodeType )
maybeAuthResultTypes authResult =
    if authResult.result == "OK" then
        List.head authResult.childtypes
            |> Maybe.map (\r -> ( authResult.nodeType, authResult.nodeId, r.nodeType ))
    else
        Nothing


fetchIfAuthorized : Container -> AuthResult -> ( Container, Cmd Msg )
fetchIfAuthorized container authResult =
    let
        maybeTypes =
            maybeAuthResultTypes authResult
    in
        case maybeTypes of
            Just ( parentType, nodeId, childType ) ->
                fetchInitialData parentType
                    nodeId
                    childType
                    { container | authResult = Success authResult }

            Nothing ->
                ( container, Cmd.none )


fetchInitialData : NodeType -> NodeId -> NodeType -> Container -> ( Container, Cmd Msg )
fetchInitialData parentType nodeId childType container =
    let
        x =
            Debug.log "fetchInitialData" ( parentType, nodeId, childType )

        treeId =
            nodeId ++ "-" ++ (nodeTypeToPath childType)

        treeCmd =
            Cmd.map TreeMsg (Tree.Commands.fetchRoot treeId)

        ( newContainer, headerCmd ) =
            Header.Commands.fetchHeader container ( parentType, nodeId, True )
    in
        ( { newContainer | path = [] }, Cmd.batch [ treeCmd, headerCmd ] )


fetchContent : Container -> Bool -> HeaderData -> ( Container, Cmd Msg )
fetchContent container isTree headerData =
    let
        ui =
            container.headerUi

        childtypes =
            if isTree then
                headerData.childtypes
            else
                container.childtypes

        newContainer =
            { container | headerData = Success headerData, childtypes = childtypes }

        headerId =
            Header.Models.headerId container.headerData

        updatedHeaderId =
            Header.Models.headerId newContainer.headerData

        updatedTab =
            getTabFromType newContainer.headerData container.tab.tabType

        ( updatedContent, cmdContent ) =
            if (headerId /= updatedHeaderId) then
                ( Loading, Content.Commands.fetchContent updatedTab.tabType updatedHeaderId )
            else
                ( newContainer.content, Cmd.none )

        cmdBatch =
            Cmd.batch
                [ Cmd.map ContentMsg cmdContent
                ]
    in
        ( { newContainer | tab = updatedTab, content = updatedContent }, cmdBatch )


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
