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
import Helpers.RemoteData exposing (..)
import Helpers.Return exposing (..)
import Return exposing (..)
import Container.Out exposing (..)


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
            Tree.Update.update (Tree.Messages.SelectNode nodeId) container.tree
                |> updatePathFromTree container

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
            Tree.Update.update subMsg container.tree
                |> updatePathFromTree container

        FetchHeaderResponse isTree model ->
            RemoteData.map (updateContent container isTree) model
                |> RemoteData.withDefault (Return.singleton container)

        FetchFoldersResponse nodeId folders ->
            ( { container | content = RemoteData.map Content.Models.FoldersContent folders }, Cmd.none )

        FetchUsersResponse nodeId users ->
            ( { container | content = RemoteData.map Content.Models.UsersContent users }, Cmd.none )

        FetchIssuesResponse nodeId issues ->
            ( { container | content = RemoteData.map Content.Models.IssuesContent issues }, Cmd.none )

        ContentMsg subMsg ->
            updateContentMsg container subMsg

        HeaderMsg subMsg ->
            updateHeaderMsg container subMsg


updateContentMsg : Container -> Content.Models.Msg -> Return Msg Container
updateContentMsg container subMsg =
    Helpers.RemoteData.update (Content.Update.update subMsg) container.content
        |> Helpers.Return.mapBoth ContentMsg (\new -> { container | content = new })
        |> unwrap


updateHeaderMsg : Container -> Header.Models.Msg -> Return Msg Container
updateHeaderMsg container subMsg =
    let
        applyOut out return =
            let
                ( ( container, cmd ), outmsgs ) =
                    return

                nodeInfo =
                    case out of
                        OutUpdateRoot method root ->
                            Just ( root.id, root.name )

                        OutUpdateCustomer method customer ->
                            Just ( customer.id, customer.name )

                        OutUpdateClient method client ->
                            Just ( client.id, client.name )

                        OutUpdateSite method site ->
                            Just ( site.id, site.name )

                        OutUpdateStaff method staff ->
                            Just ( staff.id, staff.name )

                        _ ->
                            Nothing
            in
                case nodeInfo of
                    Just ( nodeId, maybeName ) ->
                        Tree.Update.update (Tree.Messages.UpdateNode nodeId (Maybe.withDefault "" maybeName)) container.tree
                            |> updatePathFromTree container
                            |> updateContentNode nodeId (Maybe.withDefault "" maybeName)
                            |> Helpers.Return.wrap

                    _ ->
                        return

        returnout =
            Helpers.RemoteData.update (Header.Update.update subMsg) container.header
                |> Helpers.Return.mapBoth HeaderMsg (\new -> { container | header = new })
                |> mapOut applyOut
    in
        returnout |> unwrap


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


updatePathFromTree : Container -> ReturnOut Tree.Messages.Msg OutMsg (WebData Tree) -> Return Msg Container
updatePathFromTree container ( ( updatedTree, cmdTree ), outmsgs ) =
    RemoteData.map (updatePathFromTreeSuccess container cmdTree outmsgs) updatedTree
        |> RemoteData.withDefault ( container, Cmd.none )


updatePathFromTreeSuccess : Container -> Cmd Tree.Messages.Msg -> List OutMsg -> Tree -> Return Msg Container
updatePathFromTreeSuccess container cmdTree outmsgs updatedTree =
    let
        selectPath path ( container, cmd ) =
            List.head path
                |> Maybe.map (\s -> ( s.nodeType, s.id, False ))
                |> Maybe.withDefault ( updatedTree.nodeType, updatedTree.id, True )
                |> fetchHeader { container | path = path }
                |> Return.command cmd

        openRoot ( treeType, treeId ) return =
            return
                |> Return.command (goto treeType treeId)

        applyOut outmsg return =
            case outmsg of
                OutTreePath path ->
                    selectPath path return

                OutTreeRoot root ->
                    openRoot root return

                _ ->
                    return

        newReturn =
            ( { container | tree = Success updatedTree }, Cmd.map TreeMsg cmdTree )
    in
        List.foldl applyOut newReturn outmsgs


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


updateContentNode : NodeId -> String -> Return Msg Container -> Return Msg Container
updateContentNode nodeId nodeName return =
    let
        ( container, cmd ) =
            return

        subMsg =
            Content.Models.UpdateNode nodeId nodeName

        newReturn =
            Helpers.RemoteData.update (Content.Update.update subMsg) container.content
                |> Helpers.Return.mapBoth ContentMsg (\new -> { container | content = new })
                |> Helpers.Return.unwrap
    in
        newReturn |> Return.command cmd
