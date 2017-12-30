module Tree.Update exposing (..)

import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Tree.Models exposing (..)
import Tree.Commands exposing (..)
import RemoteData exposing (..)
import Helpers.Helpers
import Helpers.RemoteData
import Helpers.Return as Return exposing (..)
import Container.Out exposing (..)


update : Msg -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
update msg tree =
    let
        errorCmd =
            case msg of
                OnFetchRoot tempRoot ->
                    Helpers.Helpers.errorCmd tempRoot

                OnFetchNode nodeId tempChildren ->
                    Helpers.Helpers.errorCmd tempChildren

                _ ->
                    Cmd.none
    in
        updateInner msg tree


updateInner : Msg -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
updateInner message tree =
    case message of
        OnFetchRoot tempRoot ->
            fetchedRoot tempRoot tree |> singleton

        OnFetchNode nodeId tempChildren ->
            fetchedNode nodeId tempChildren tree |> singleton

        ToggleNode nodeId ->
            toggle nodeId tree

        InsertNode parentId nodeId nodeType childrenState name ->
            insertTree parentId nodeId nodeType childrenState name tree

        UpdateNode nodeId name ->
            updateTree nodeId name tree

        SelectRoot ->
            RemoteData.map (\t -> select t.id tree) tree
                |> RemoteData.withDefault (singleton tree)

        SelectNode nodeId ->
            select nodeId tree

        OpenNewRoot nodeType nodeId ->
            select nodeId tree
                |> dropout
                |> outmsg (OutTreeRoot ( nodeType, nodeId ))

        NoAction ->
            tree |> singleton


selectNode : NodeId -> List Node -> Node -> ( Node, List Node )
selectNode nodeId path node =
    let
        defaultNode =
            let
                newNode =
                    { node | selected = node.id == nodeId }

                newPath =
                    List.filter (\n -> n.selected) [ newNode ]
            in
                ( newNode, newPath )
    in
        RemoteData.map (selectNodeSuccess nodeId node path) node.childNodes
            |> RemoteData.withDefault defaultNode


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


deselect : NodeId -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
deselect nodeId tree =
    let
        treeUpdate tree =
            singleton { tree | selected = False }

        nodeUpdate node =
            singleton { node | selected = False }

        ( ( newTree1, _ ), _ ) =
            traverseTree nodeId treeUpdate nodeUpdate tree

        newTree2 =
            RemoteData.map (\t -> { t | path = [] }) newTree1
    in
        singleton newTree2


select : NodeId -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
select nodeId webdata =
    let
        selectedId tree =
            case tree.path of
                head :: _ ->
                    head.id

                _ ->
                    tree.id

        treeUpdate tree =
            singleton { tree | selected = True }

        nodeUpdate node =
            singleton { node | selected = True }

        success nodeId tree =
            let
                currentId =
                    selectedId tree
            in
                if nodeId == currentId then
                    singleton (Success tree)
                else
                    let
                        ( ( newTree, _ ), _ ) =
                            deselect currentId (Success tree)

                        applyPath path webdata =
                            RemoteData.map (\tree -> { tree | path = path }) webdata

                        applyOut out return =
                            case out of
                                OutTreePath path ->
                                    return |> Return.map (applyPath path)

                                _ ->
                                    return
                    in
                        traverseTree nodeId treeUpdate nodeUpdate newTree
                            |> mapOut applyOut
    in
        RemoteData.map (success nodeId) webdata
            |> RemoteData.withDefault (singleton webdata)


toggle : NodeId -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
toggle nodeId tree =
    let
        treeUpdate tree =
            singleton tree

        nodeUpdate node =
            case node.childrenState of
                NoChildren ->
                    singleton node

                Collapsed ->
                    if node.childNodes == NotAsked then
                        return { node | childrenState = Expanding } (Tree.Commands.fetchNode node.id)
                    else
                        singleton { node | childrenState = Expanded }

                Expanding ->
                    singleton node

                Expanded ->
                    singleton { node | childrenState = Collapsed }

                RootNode ->
                    return { node | childrenState = Expanding } (Tree.Commands.fetchRoot node.id)
    in
        traverseTree nodeId treeUpdate nodeUpdate tree
            |> dropout


insertNode : Node -> List Node -> List Node
insertNode node children =
    let
        compare n1 n2 =
            (String.toLower n1.name) > (String.toLower n2.name)

        before =
            List.filter (compare node) children

        after =
            List.filter (not << compare node) children
    in
        before ++ [ node ] ++ after


insertTree : NodeId -> NodeId -> NodeType -> ChildrenState -> String -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
insertTree parentId nodeId nodeType childrenState name tree =
    let
        newNode =
            { id = nodeId
            , nodeType = nodeType
            , name = name
            , selected = False
            , childrenState = childrenState
            , childNodes = NotAsked
            , rootType = nodeType
            }

        treeUpdate tree =
            let
                (ChildNodes children) =
                    tree.childNodes
            in
                singleton { tree | childNodes = ChildNodes (insertNode newNode children) }

        nodeUpdate node =
            let
                success childNodes =
                    let
                        (ChildNodes children) =
                            childNodes
                    in
                        ChildNodes (insertNode newNode children)

                newChildNodes =
                    RemoteData.map success node.childNodes
            in
                singleton { node | childNodes = newChildNodes }
    in
        traverseTree parentId treeUpdate nodeUpdate tree


updateTree : NodeId -> String -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
updateTree nodeId name tree =
    let
        treeUpdate tree =
            singleton { tree | name = name }

        nodeUpdate node =
            singleton { node | name = name }
    in
        traverseTree nodeId treeUpdate nodeUpdate tree
            |> logout



--|> dropout


traverseChildNodes : NodeId -> (Node -> ReturnOut Msg OutMsg Node) -> Node -> ReturnOut Msg OutMsg Node
traverseChildNodes nodeId updateNodeFn node =
    let
        applyOut out return =
            case out of
                OutTreePath path ->
                    return
                        |> dropout
                        |> outmsg (OutTreePath (path ++ [ node ]))

                _ ->
                    return

        success nodeId updateNodeFn node (ChildNodes childNodes) =
            List.map (traverseNode nodeId updateNodeFn) childNodes
                |> Return.sequence
                |> Return.mapOut applyOut
                |> Return.map (\nodes -> { node | childNodes = Success (ChildNodes nodes) })
    in
        RemoteData.map (success nodeId updateNodeFn node) node.childNodes
            |> RemoteData.withDefault (singleton node)


traverseNode : NodeId -> (Node -> ReturnOut Msg OutMsg Node) -> Node -> ReturnOut Msg OutMsg Node
traverseNode nodeId updateNodeFn node =
    if nodeId == node.id then
        let
            returnOut =
                updateNodeFn node

            ( ( newNode, _ ), _ ) =
                returnOut
        in
            returnOut |> outmsg (OutTreePath [ newNode ])
    else
        traverseChildNodes nodeId updateNodeFn node


traverseTree : NodeId -> (Tree -> ReturnOut Msg OutMsg Tree) -> (Node -> ReturnOut Msg OutMsg Node) -> WebData Tree -> ReturnOut Msg OutMsg (WebData Tree)
traverseTree nodeId updateTreeFn updateNodeFn tree =
    let
        success nodeId updateTreeFn updateNodeFn tree =
            if nodeId == tree.id then
                updateTreeFn tree |> outmsg (OutTreePath [])
            else
                let
                    (ChildNodes childNodes) =
                        tree.childNodes
                in
                    List.map (traverseNode nodeId updateNodeFn) childNodes
                        |> Return.sequence
                        |> Return.map (\nodes -> { tree | childNodes = ChildNodes nodes })
    in
        Helpers.RemoteData.update (success nodeId updateTreeFn updateNodeFn) tree


createNode : TempNode -> Node
createNode tempNode =
    let
        nodeType =
            Maybe.withDefault RootType (convertNodeType tempNode.type_)

        rootType =
            Maybe.withDefault RootType (convertNodeType tempNode.rootType)
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
        , rootType = rootType
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
