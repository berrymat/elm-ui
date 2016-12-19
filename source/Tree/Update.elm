module Tree.Update exposing (..)

import Tree.Messages exposing (Msg(..))
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Commands exposing (..)
import RemoteData exposing (..)


selectNode : NodeId -> List Node -> Node -> ( Node, List Node )
selectNode nodeId path node =
    RemoteData.map (selectNodeSuccess nodeId node path) node.childNodes
        |> RemoteData.withDefault ( node, path )


selectNodeSuccess : NodeId -> Node -> List Node -> ChildNodes -> ( Node, List Node )
selectNodeSuccess nodeId node path (ChildNodes childNodes) =
    let
        results =
            List.map (selectNode nodeId path) childNodes

        ( newChildNodes, newPathLists ) =
            List.unzip results

        newNode =
            { node | childNodes = Success (ChildNodes newChildNodes), selected = node.id == nodeId }

        newPath =
            if newNode.selected then
                [ newNode ]
            else
                let
                    concatLists =
                        List.concat newPathLists
                in
                    if List.isEmpty concatLists then
                        concatLists
                    else
                        List.append concatLists [ newNode ]
    in
        ( newNode, newPath )


remoteMaybe : (a -> ( b, c )) -> RemoteData e a -> ( RemoteData e b, Maybe c )
remoteMaybe f remoteData =
    case remoteData of
        Success data ->
            let
                ( first, second ) =
                    f data
            in
                ( Success first, Just second )

        NotAsked ->
            ( NotAsked, Nothing )

        Loading ->
            ( Loading, Nothing )

        Failure error ->
            ( Failure error, Nothing )


select : Maybe NodeId -> WebData Tree -> ( WebData Tree, Maybe (List Node) )
select maybeNodeId tree =
    remoteMaybe (selectSuccess maybeNodeId) tree


selectSuccess : Maybe NodeId -> Tree -> ( Tree, List Node )
selectSuccess maybeNodeId tree =
    let
        (ChildNodes childNodes) =
            tree.childNodes

        nodeId =
            Maybe.withDefault tree.id maybeNodeId

        results =
            List.map (selectNode nodeId tree.path) childNodes

        ( newChildren, newPathLists ) =
            List.unzip results

        newPath =
            List.concat newPathLists

        newSelected =
            (List.length newPath) == 0
    in
        ( { tree | childNodes = (ChildNodes newChildren), selected = newSelected, path = newPath }, newPath )


toggleChildNodes : NodeId -> Node -> ( Node, Cmd Msg )
toggleChildNodes nodeId node =
    RemoteData.map (toggleChildNodesSuccess nodeId node) node.childNodes
        |> RemoteData.withDefault ( node, Cmd.none )


toggleChildNodesSuccess : NodeId -> Node -> ChildNodes -> ( Node, Cmd Msg )
toggleChildNodesSuccess nodeId node (ChildNodes childNodes) =
    let
        results =
            List.map (toggleNode nodeId) childNodes

        ( newChildNodes, cmds ) =
            List.unzip results
    in
        ( { node | childNodes = Success (ChildNodes newChildNodes) }, Cmd.batch cmds )


toggleNode : NodeId -> Node -> ( Node, Cmd Msg )
toggleNode nodeId node =
    let
        ( newNode, cmd ) =
            if nodeId == node.id then
                case node.childrenState of
                    NoChildren ->
                        ( node, Cmd.none )

                    Collapsed ->
                        if node.childNodes == NotAsked then
                            ( { node | childrenState = Expanding }, Tree.Commands.fetchNode node.id )
                        else
                            ( { node | childrenState = Expanded }, Cmd.none )

                    Expanding ->
                        ( node, Cmd.none )

                    Expanded ->
                        ( { node | childrenState = Collapsed }, Cmd.none )

                    RootNode ->
                        ( { node | childrenState = Expanding }, Tree.Commands.fetchRoot node.id )
            else
                toggleChildNodes nodeId node
    in
        ( newNode, cmd )


toggle : NodeId -> WebData Tree -> ( WebData Tree, Cmd Msg )
toggle nodeId tree =
    RemoteData.update (toggleSuccess nodeId) tree


toggleSuccess : NodeId -> Tree -> ( Tree, Cmd Msg )
toggleSuccess nodeId tree =
    let
        (ChildNodes childNodes) =
            tree.childNodes

        results =
            List.map (toggleNode nodeId) childNodes

        ( newChildren, cmds ) =
            List.unzip results
    in
        ( { tree | childNodes = (ChildNodes newChildren) }, Cmd.batch cmds )


createNode : TempNode -> Node
createNode tempNode =
    let
        nodeType =
            Maybe.withDefault RootType (convertNodeType tempNode.type_)
    in
        { id = tempNode.id
        , nodeType = nodeType
        , name = tempNode.name
        , selected = False
        , childrenState =
            if tempNode.hasChildren then
                Collapsed
            else if tempNode.isRoot then
                RootNode
            else
                NoChildren
        , childNodes = NotAsked
        }


fetchedRoot : WebData TempRoot -> WebData Tree -> WebData Tree
fetchedRoot tempRoot tree =
    RemoteData.map fetchedRootSuccess tempRoot


fetchedRootSuccess : TempRoot -> Tree
fetchedRootSuccess tempRoot =
    let
        nodeType =
            Maybe.withDefault RootType (convertNodeType tempRoot.type_)
    in
        { id = tempRoot.id
        , nodeType = nodeType
        , name = tempRoot.name
        , selected = True
        , childrenState = Expanded
        , childNodes = (ChildNodes (List.map createNode tempRoot.children))
        , path = []
        }


convertChildren : TempChildren -> List Node
convertChildren tempChildren =
    List.map createNode tempChildren.children


expandChildren : NodeId -> WebData TempChildren -> Node -> Node
expandChildren nodeId tempChildren node =
    if nodeId == node.id then
        let
            convertedChildren =
                RemoteData.map convertChildren tempChildren

            newChildNodes =
                RemoteData.map ChildNodes convertedChildren
        in
            { node | childrenState = Expanded, childNodes = newChildNodes }
    else
        RemoteData.map (expandChildrenSuccess nodeId tempChildren node) node.childNodes
            |> RemoteData.withDefault node


expandChildrenSuccess : NodeId -> WebData TempChildren -> Node -> ChildNodes -> Node
expandChildrenSuccess nodeId tempChildren node (ChildNodes childNodes) =
    let
        newChildNodes =
            List.map (expandChildren nodeId tempChildren) childNodes
    in
        { node | childNodes = Success (ChildNodes newChildNodes) }


fetchedNode : NodeId -> WebData TempChildren -> WebData Tree -> WebData Tree
fetchedNode nodeId tempChildren tree =
    RemoteData.map (fetchedNodeSuccess nodeId tempChildren) tree


fetchedNodeSuccess : NodeId -> WebData TempChildren -> Tree -> Tree
fetchedNodeSuccess nodeId tempChildren tree =
    let
        (ChildNodes childNodes) =
            tree.childNodes

        newChildren =
            List.map (expandChildren nodeId tempChildren) childNodes
    in
        { tree | childNodes = (ChildNodes newChildren) }


update : Msg -> WebData Tree -> ( WebData Tree, Cmd Msg, Maybe (List Node), Maybe ( NodeType, NodeId ) )
update message tree =
    case message of
        OnFetchRoot tempRoot ->
            ( fetchedRoot tempRoot tree, Cmd.none, Nothing, Nothing )

        OnFetchNode nodeId tempChildren ->
            ( fetchedNode nodeId tempChildren tree, Cmd.none, Nothing, Nothing )

        ToggleNode nodeId ->
            let
                ( newTree, cmds ) =
                    toggle nodeId tree
            in
                ( newTree, cmds, Nothing, Nothing )

        SelectRoot ->
            let
                ( newTree, newPath ) =
                    select Nothing tree
            in
                ( newTree, Cmd.none, newPath, Nothing )

        SelectNode nodeId ->
            let
                ( newTree, newPath ) =
                    select (Just nodeId) tree
            in
                ( newTree, Cmd.none, newPath, Nothing )

        SelectNewRoot nodeType nodeId ->
            let
                ( newTree, newPath ) =
                    select (Just nodeId) tree
            in
                ( newTree, fetchRoot nodeId, newPath, Just ( nodeType, nodeId ) )
