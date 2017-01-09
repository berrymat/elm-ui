module View exposing (..)

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Ui.App
import Ui
import Ui.Layout
import Ui.Header
import Messages exposing (Msg(..))
import Models exposing (Model)
import Login.Models
import Login.View
import Container.View
import Container.Messages
import Routing exposing (Route(..))
import Container.Models exposing (..)
import RemoteData
import Helpers.Models exposing (..)


view : Model -> Html Msg
view model =
    Ui.App.view
        App
        model.app
        [ Ui.Layout.website
            [ nav model ]
            [ page model ]
            [ footer model ]
        ]


containerNavItems : Model -> List (Html Msg)
containerNavItems model =
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
                , action = Just (ContainerMsg (Container.Messages.Goto entity.nodeType headerId))
                , link = Nothing
                , target = ""
                }

        childitems =
            List.map headeritem container.childtypes
                |> List.intersperse (Ui.Header.separator)
    in
        [ Ui.Header.item
            { text = "Home"
            , action = Just (ContainerMsg Container.Messages.GotoHome)
            , link = Nothing
            , target = ""
            }
        , Ui.Header.separator
        ]
            ++ childitems
            ++ [ Ui.spacer
               , Ui.Header.separator
               , Ui.Header.item
                    { text = "Logout"
                    , action = Just Logout
                    , link = Nothing
                    , target = ""
                    }
               ]


loginNavItems : Model -> List (Html Msg)
loginNavItems model =
    [ Ui.spacer
    , Ui.Header.separator
    , Ui.Header.item
        { text = "Login"
        , action = Just (LoginMsg Login.Models.OpenLoginModal)
        , link = Nothing
        , target = ""
        }
    ]


navItems : Model -> List (Html Msg)
navItems model =
    case model.route of
        LoginRoute ->
            loginNavItems model

        HomeRoute ->
            containerNavItems model

        CustomerRoute id ->
            containerNavItems model

        ClientRoute id ->
            containerNavItems model

        StaffRoute id ->
            containerNavItems model

        NotFoundRoute ->
            []


nav : Model -> Html Msg
nav model =
    Ui.Header.view
        (navItems model)


footer : Model -> Html Msg
footer model =
    div [ class "footer clearfix p1" ]
        [ text "Elm prototype" ]


authToken : Model -> AuthToken
authToken model =
    RemoteData.map .authToken model.login.authResult
        |> RemoteData.withDefault ""


page : Model -> Html Msg
page model =
    case model.route of
        LoginRoute ->
            loginPage model.login

        HomeRoute ->
            containerPage model.container (authToken model)

        CustomerRoute id ->
            containerPage model.container (authToken model)

        ClientRoute id ->
            containerPage model.container (authToken model)

        StaffRoute id ->
            containerPage model.container (authToken model)

        NotFoundRoute ->
            notFoundView


loginPage : Login.Models.Login -> Html Msg
loginPage login =
    Html.map LoginMsg (Login.View.view login)


containerPage : Container -> AuthToken -> Html Msg
containerPage container token =
    Html.map ContainerMsg (Container.View.view container token)


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
