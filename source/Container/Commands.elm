module Container.Commands exposing (..)

import Container.Models exposing (..)
import Helpers.Models exposing (..)
import Folders.Commands
import Issues.Models
import Users.Models
import Helpers.Helpers exposing (..)
import RemoteData exposing (..)
import Header.Models exposing (Header(..))
import Header.Update exposing (..)
import Return exposing (..)


headerId : WebData Header.Models.Model -> NodeId
headerId model =
    RemoteData.map Header.Models.headerId model
        |> RemoteData.withDefault ""


getTabFromType : TabType -> WebData Header.Models.Model -> Tab
getTabFromType tabType model =
    RemoteData.map (Header.Models.getTabFromType tabType) model
        |> RemoteData.withDefault (Tab EmptyTab "")


fetchHeader : Container -> ( NodeType, NodeId, Bool ) -> Return Msg Container
fetchHeader container ( nodeType, nodeId, isTree ) =
    let
        cmd =
            if nodeId /= "" && nodeId /= (headerId container.header) then
                case nodeType of
                    RootType ->
                        fetchRoot nodeId isTree

                    CustomerType ->
                        fetchCustomer nodeId isTree

                    ClientType ->
                        fetchClient nodeId isTree

                    SiteType ->
                        fetchSite nodeId isTree

                    StaffType ->
                        fetchStaff nodeId isTree

                    FolderType ->
                        Cmd.none
            else
                Cmd.none

        newHeader =
            if cmd /= Cmd.none then
                Loading
            else
                container.header
    in
        ( { container | header = newHeader }, cmd )


fetchContent : TabType -> NodeId -> Cmd Msg
fetchContent tabType nodeId =
    if nodeId /= "" then
        case tabType of
            FoldersType ->
                fetchFolders nodeId

            UsersType ->
                fetchUsers nodeId

            IssuesType ->
                fetchIssues nodeId

            EmptyTab ->
                Cmd.none
    else
        Cmd.none


fetchFolders : NodeId -> Cmd Msg
fetchFolders nodeId =
    fetcher "Folders"
        nodeId
        Folders.Commands.foldersDecoder
        ((FetchFoldersResponse nodeId) << RemoteData.fromResult)


fetchUsers : NodeId -> Cmd Msg
fetchUsers nodeId =
    fetcher "Users"
        nodeId
        Users.Models.modelDecoder
        ((FetchUsersResponse nodeId) << RemoteData.fromResult)


fetchIssues : NodeId -> Cmd Msg
fetchIssues nodeId =
    fetcher "Issues"
        nodeId
        Issues.Models.modelDecoder
        ((FetchIssuesResponse nodeId) << RemoteData.fromResult)



{-
   deleteHeader : AuthToken -> NodeId -> Model -> Cmd Msg
   deleteHeader token nodeId model =
       case model.header of
           RootHeader root ->
               deleteRoot token nodeId root

           CustomerHeader customer ->
               deleteCustomer token nodeId customer

           ClientHeader client ->
               deleteClient token nodeId client

           SiteHeader site ->
               deleteSite token nodeId site

           StaffHeader staff ->
               deleteStaff token nodeId staff

           Header.Models.Empty ->
               Cmd.none
-}


fetchRoot : NodeId -> Bool -> Cmd Msg
fetchRoot nodeId isTree =
    fetcher "Roots" nodeId (modelDecoder rootDecoder) ((FetchHeaderResponse isTree) << RemoteData.fromResult)



{-
   deleteRoot : AuthToken -> NodeId -> Root -> Cmd Msg
   deleteRoot token nodeId root =
       requester token "Roots" nodeId Delete (encodeRoot root) rootDecoder (HeaderSaveResponse << RemoteData.fromResult)
-}


fetchCustomer : NodeId -> Bool -> Cmd Msg
fetchCustomer nodeId isTree =
    fetcher "Customers" nodeId (modelDecoder customerDecoder) ((FetchHeaderResponse isTree) << RemoteData.fromResult)



{-
   deleteCustomer : AuthToken -> NodeId -> Customer -> Cmd Msg
   deleteCustomer token nodeId customer =
       requester token "Customers" nodeId Delete (encodeCustomer customer) customerDecoder (HeaderSaveResponse << RemoteData.fromResult)
-}


fetchClient : NodeId -> Bool -> Cmd Msg
fetchClient nodeId isTree =
    fetcher "Clients" nodeId (modelDecoder clientDecoder) ((FetchHeaderResponse isTree) << RemoteData.fromResult)



{-
   deleteClient : AuthToken -> NodeId -> Client -> Cmd Msg
   deleteClient token nodeId client =
       requester token "Clients" nodeId Delete (encodeClient client) clientDecoder (HeaderSaveResponse << RemoteData.fromResult)
-}


fetchSite : NodeId -> Bool -> Cmd Msg
fetchSite nodeId isTree =
    fetcher "Sites" nodeId (modelDecoder siteDecoder) ((FetchHeaderResponse isTree) << RemoteData.fromResult)



{-
   deleteSite : AuthToken -> NodeId -> Site -> Cmd Msg
   deleteSite token nodeId site =
       requester token "Sites" nodeId Delete (encodeSite site) siteDecoder (HeaderSaveResponse << RemoteData.fromResult)
-}


fetchStaff : NodeId -> Bool -> Cmd Msg
fetchStaff nodeId isTree =
    fetcher "Staff" nodeId (modelDecoder staffDecoder) ((FetchHeaderResponse isTree) << RemoteData.fromResult)



{-
   deleteStaff : AuthToken -> NodeId -> Staff -> Cmd Msg
   deleteStaff token nodeId staff =
       requester token "Staff" nodeId Delete (encodeStaff staff) staffDecoder (HeaderSaveResponse << RemoteData.fromResult)
-}
