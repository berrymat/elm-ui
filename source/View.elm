module View exposing (..)

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Ui.App
import Ui
import Ui.Layout
import Ui.Header
import Models exposing (..)
import Login.Models
import Login.View
import Container.View
import Container.Models
import Reset.View
import Reset.Models
import Routing exposing (Route(..))
import RemoteData
import Helpers.Models exposing (..)
import Ui.NotificationCenter


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
                , action = Just (ContainerMsg (Container.Models.Goto entity.nodeType headerId))
                , link = Nothing
                , target = ""
                }

        childitems =
            List.map headeritem container.childtypes
                |> List.intersperse (Ui.Header.separator)
    in
        [ Ui.Header.item
            { text = "Home"
            , action = Just (ContainerMsg Container.Models.GotoHome)
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

        ResetRoute resetToken ->
            loginNavItems model

        NotFoundRoute ->
            []


nav : Model -> Html Msg
nav model =
    Ui.Header.view
        (navItems model)


footer : Model -> Html Msg
footer model =
    div [ class "footer clearfix p1" ]
        [ text "Elm prototype"
        , Ui.NotificationCenter.view NotificationMsg model.notificationCenter
        ]


authToken : Model -> AuthToken
authToken model =
    RemoteData.map .authToken model.login.authResult
        |> RemoteData.withDefault ""


resetAuthToken : Model -> AuthToken
resetAuthToken model =
    RemoteData.map .authToken model.reset.authResult
        |> RemoteData.withDefault ""


page : Model -> Html Msg
page model =
    case model.route of
        LoginRoute ->
            loginPage model.login

        HomeRoute ->
            containerPage (authToken model) model.container

        CustomerRoute id ->
            containerPage (authToken model) model.container

        ClientRoute id ->
            containerPage (authToken model) model.container

        StaffRoute id ->
            containerPage (authToken model) model.container

        ResetRoute resetToken ->
            resetPage (resetAuthToken model) model.reset

        NotFoundRoute ->
            notFoundView


loginPage : Login.Models.Login -> Html Msg
loginPage login =
    Html.map LoginMsg (Login.View.view login)


containerPage : AuthToken -> Container.Models.Container -> Html Msg
containerPage token container =
    Html.map ContainerMsg (Container.View.view token container)


resetPage : AuthToken -> Reset.Models.Model -> Html Msg
resetPage token reset =
    Html.map ResetMsg (Reset.View.view token reset)


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]


viewNotification : Notification -> Html Msg
viewNotification notification =
    div
        [ class ("notification-" ++ notification.notificationType) ]
        [ text notification.message ]
