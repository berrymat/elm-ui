module Container.Commands exposing (..)

import Container.Messages exposing (..)
import Container.Models exposing (..)
import Tree.Models exposing (NodeId, NodeType, Node, ChildrenState(..))
import Header.Models exposing (isHeaderEmpty, getTabFromType, HeaderData)
import Tree.Commands
import Header.Commands
import Content.Commands
import Helpers.Helpers exposing (apiUrl)
import Erl
import Http
import Json.Decode as Decode exposing (field)
import HttpBuilder exposing (..)
import RemoteData exposing (..)


{-
   authenticate : String -> String -> NodeType -> NodeId -> Cmd Msg
   authenticate username password nodeType nodeId =
       Http.get (authenticateUrl username password) (authenticateDecoder nodeType nodeId)
           |> Http.send OnAuthenticate
-}


authenticate : String -> String -> NodeType -> NodeId -> Cmd Msg
authenticate username password nodeType nodeId =
    HttpBuilder.get (authenticateUrl username password)
        |> withExpect (Http.expectJson (authenticateDecoder nodeType nodeId))
        |> withCredentials
        |> send (OnAuthenticate << RemoteData.fromResult)


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
    Decode.map (AuthResult nodeType nodeId)
        Decode.string


fetchIfAuthorized : Container -> AuthResult -> ( Container, Cmd Msg )
fetchIfAuthorized container authResult =
    if authResult.result == "OK" then
        fetchInitialData authResult.nodeType authResult.nodeId container
    else
        ( container, Cmd.none )


fetchInitialData : NodeType -> NodeId -> Container -> ( Container, Cmd Msg )
fetchInitialData nodeType nodeId container =
    let
        treeCmd =
            if container.tree.childrenState == NoChildren then
                Cmd.map TreeMsg (Tree.Commands.fetchRoot nodeId)
            else
                Cmd.none

        ( newHeaderInfo, headerCmd ) =
            if (isHeaderEmpty container.headerInfo) then
                Header.Commands.fetchHeader container.headerInfo nodeType nodeId
            else
                ( container.headerInfo, Cmd.none )
    in
        ( { container | headerInfo = newHeaderInfo }, Cmd.batch [ treeCmd, headerCmd ] )


fetchContent : Container -> HeaderData -> ( Container, Cmd Msg )
fetchContent container headerData =
    let
        headerInfo =
            container.headerInfo

        newHeaderInfo =
            { headerInfo | data = Success headerData }

        headerId =
            Header.Models.headerId container.headerInfo

        updatedHeaderId =
            Header.Models.headerId newHeaderInfo

        updatedTab =
            getTabFromType newHeaderInfo container.tab.tabType

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
        ( { container | headerInfo = newHeaderInfo, tab = updatedTab }, cmdBatch )
