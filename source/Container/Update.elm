module Container.Update exposing (..)

import Container.Commands exposing (..)
import Container.Models exposing (..)
import Content.Models
import Tree.Messages
import Tree.Commands
import Tree.Update
import Tree.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Header.Models exposing (getTabFromType)
import Content.Update
import Header.Update
import Navigation
import RemoteData exposing (..)
import Return exposing (..)


update : Msg -> Container -> Return Msg Container
update msg container =
    let
        errorCmd =
            case msg of
                FetchHeaderResponse isTree model ->
                    Helpers.Helpers.errorCmd model

                FetchFoldersResponse nodeId folders ->
                    Helpers.Helpers.errorCmd folders

                FetchUsersResponse nodeId users ->
                    Helpers.Helpers.errorCmd users

                FetchIssuesResponse nodeId issues ->
                    Helpers.Helpers.errorCmd issues

                _ ->
                    Cmd.none

        return =
            updateInner msg container
    in
        (return |> Return.command errorCmd)


updateInner : Msg -> Container -> Return Msg Container
updateInner msg container =
    case msg of
        GotoHome ->
            updateGotoHome container

        Goto nodeType nodeId ->
            updateGoto container nodeType nodeId

        LoadContainer parentType nodeId childType ->
            updateLoadContainer parentType nodeId childType container

        SelectPath nodeId ->
            let
                ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                    Tree.Update.update (Tree.Messages.SelectNode nodeId) container.tree
            in
                updatePathFromTree container cmdTree maybePath maybeRoot updatedTree

        SelectTab tabType ->
            let
                nodeId =
                    Container.Commands.headerId container.header

                updatedTab =
                    Container.Commands.getTabFromType tabType container.header

                cmdContent =
                    fetchContent tabType nodeId
            in
                ( { container | tab = updatedTab, content = Loading }, cmdContent )

        TreeMsg subMsg ->
            let
                ( ( updatedTree, cmdTree ), maybePath, maybeRoot ) =
                    Tree.Update.update subMsg container.tree
            in
                updatePathFromTree container cmdTree maybePath maybeRoot updatedTree

        FetchHeaderResponse isTree model ->
            RemoteData.map (updateContent container isTree) model
                |> RemoteData.withDefault (singleton container)

        FetchFoldersResponse nodeId folders ->
            ( { container | content = RemoteData.map Content.Models.FoldersContent folders }, Cmd.none )

        FetchUsersResponse nodeId users ->
            ( { container | content = RemoteData.map Content.Models.UsersContent users }, Cmd.none )

        FetchIssuesResponse nodeId issues ->
            ( { container | content = RemoteData.map Content.Models.IssuesContent issues }, Cmd.none )

        ContentMsg subMsg ->
            RemoteData.update (Content.Update.update subMsg) container.content
                |> Return.mapBoth ContentMsg (\new -> { container | content = new })

        HeaderMsg subMsg ->
            RemoteData.update (Header.Update.update subMsg) container.header
                |> Return.mapBoth HeaderMsg (\new -> { container | header = new })


subscriptions : Container -> Sub Msg
subscriptions container =
    let
        subHeader =
            RemoteData.map Header.Update.subscriptions container.header
                |> RemoteData.withDefault Sub.none

        subContent =
            RemoteData.map Content.Update.subscriptions container.content
                |> RemoteData.withDefault Sub.none
    in
        Sub.batch
            [ Sub.map HeaderMsg subHeader
            , Sub.map ContentMsg subContent
            ]


updateGotoHome : Container -> Return Msg Container
updateGotoHome container =
    ( container, Navigation.newUrl "#Home" )


updateGoto : Container -> NodeType -> NodeId -> Return Msg Container
updateGoto container nodeType nodeId =
    ( container, goto nodeType nodeId )


goto : NodeType -> NodeId -> Cmd Msg
goto nodeType nodeId =
    let
        path =
            nodeTypeToPath nodeType
    in
        Navigation.newUrl ("#" ++ path ++ "/" ++ nodeId)


updateLoadContainer : NodeType -> NodeId -> NodeType -> Container -> Return Msg Container
updateLoadContainer parentType nodeId childType container =
    let
        treeId =
            nodeId ++ "-" ++ (nodeTypeToPath childType)

        treeCmd =
            Cmd.map TreeMsg (Tree.Commands.fetchRoot treeId)

        ( newContainer, headerCmd ) =
            fetchHeader container ( parentType, nodeId, True )
    in
        ( { newContainer | path = [] }, Cmd.batch [ treeCmd, headerCmd ] )


updatePathFromTree : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> WebData Tree -> Return Msg Container
updatePathFromTree container cmdTree maybePath maybeRoot updatedTree =
    RemoteData.map (updatePathFromTreeSuccess container cmdTree maybePath maybeRoot) updatedTree
        |> RemoteData.withDefault ( container, Cmd.none )


updatePathFromTreeSuccess : Container -> Cmd Tree.Messages.Msg -> Maybe (List Node) -> Maybe ( NodeType, NodeId ) -> Tree -> Return Msg Container
updatePathFromTreeSuccess container cmdTree maybePath maybeRoot updatedTree =
    let
        ( newContainer, cmdHeader ) =
            case maybePath of
                Just path ->
                    List.head path
                        |> Maybe.map (\s -> ( s.nodeType, s.id, False ))
                        |> Maybe.withDefault ( updatedTree.nodeType, updatedTree.id, True )
                        |> fetchHeader { container | path = path }

                Nothing ->
                    ( container, Cmd.none )

        cmdRoot =
            case maybeRoot of
                Just ( treeType, treeId ) ->
                    goto treeType treeId

                Nothing ->
                    Cmd.none

        cmdBatch =
            Cmd.batch
                [ Cmd.map TreeMsg cmdTree
                , cmdHeader
                , cmdRoot
                ]
    in
        ( { newContainer | tree = Success updatedTree }, cmdBatch )


updateContent : Container -> Bool -> Header.Models.Model -> Return Msg Container
updateContent container isTree model =
    let
        childtypes =
            if isTree then
                model.childtypes
            else
                container.childtypes

        newContainer =
            { container | header = Success model, childtypes = childtypes }

        headerId =
            RemoteData.map Header.Models.headerId container.header
                |> RemoteData.withDefault ""

        updatedHeaderId =
            Header.Models.headerId model

        updatedTab =
            Header.Models.getTabFromType container.tab.tabType model

        ( updatedContent, cmdContent ) =
            if (headerId /= updatedHeaderId) then
                ( Loading, fetchContent updatedTab.tabType updatedHeaderId )
            else
                ( newContainer.content, Cmd.none )

        cmdBatch =
            Cmd.batch
                [ cmdContent
                ]
    in
        ( { newContainer | tab = updatedTab, content = updatedContent }, cmdBatch )
