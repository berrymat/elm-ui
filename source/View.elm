module View exposing (..)

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Ui.App
import Ui
import Ui.Layout
import Ui.Header
import Messages exposing (Msg(..))
import Models exposing (Model)
import Container.View
import Container.Messages exposing (Msg(..))
import Routing exposing (Route(..))
import Container.Models exposing (..)
import RemoteData


view : Model -> Html Messages.Msg
view model =
    Ui.App.view
        App
        model.app
        [ Ui.Layout.website
            [ nav model ]
            [ page model ]
            [ footer model ]
        ]


nav : Model -> Html Messages.Msg
nav model =
    let
        container =
            model.container

        strippedId treeId =
            String.split "-" treeId
                |> List.reverse
                |> List.tail
                |> Maybe.withDefault []
                |> List.reverse
                |> String.join "-"

        headerId =
            RemoteData.map (\t -> strippedId t.id) container.tree
                |> RemoteData.withDefault ""

        headeritem entity =
            Ui.Header.item
                { text = entity.name
                , action = Just (ContainerMsg (Goto entity.nodeType headerId))
                , link = Nothing
                , target = ""
                }

        childitems =
            List.map headeritem container.childtypes
                |> List.intersperse (Ui.Header.separator)

        items =
            [ Ui.Header.item
                { text = "Home"
                , action = Just (ContainerMsg GotoHome)
                , link = Nothing
                , target = ""
                }
            , Ui.Header.separator
            ]
                ++ childitems
                ++ [ Ui.spacer
                   , Ui.Header.separator
                   , Ui.Header.icon
                        { glyph = "navicon-round"
                        , action = Nothing
                        , link = Nothing
                        , target = ""
                        , size = 24
                        }
                   ]
    in
        Ui.Header.view
            items


footer : Model -> Html Messages.Msg
footer model =
    div [ class "footer clearfix p1" ]
        [ text "Elm prototype" ]


page : Model -> Html Messages.Msg
page model =
    case model.route of
        HomeRoute ->
            containerPage model.container

        CustomerRoute id ->
            containerPage model.container

        ClientRoute id ->
            containerPage model.container

        StaffRoute id ->
            containerPage model.container

        NotFoundRoute ->
            notFoundView


containerPage : Container -> Html Messages.Msg
containerPage container =
    Html.map ContainerMsg (Container.View.view container)


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
