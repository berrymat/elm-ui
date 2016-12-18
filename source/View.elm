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
import Header.Models
import RemoteData exposing (..)


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

        headerId =
            Header.Models.headerId container.headerData

        headeritem tab =
            Ui.Header.item
                { text = tab.name
                , action = Just (ContainerMsg ShowContainer)
                , link = Nothing
                , target = ""
                }

        childitems =
            case container.headerData of
                Success data ->
                    List.map headeritem data.childtypes
                        |> List.intersperse (Ui.Header.separator)

                _ ->
                    []

        items =
            [ Ui.Header.item
                { text = "Home"
                , action = Just (ContainerMsg ShowContainer)
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
        ContainerRoute type_ id ->
            Html.map ContainerMsg
                (Container.View.view
                    model.container
                )

        ContainerRoot ->
            Html.map ContainerMsg
                (Container.View.view
                    model.container
                )

        NotFoundRoute ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
