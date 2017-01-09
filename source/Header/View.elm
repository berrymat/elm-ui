module Header.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Container.Messages exposing (..)
import Container.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Header.Models exposing (..)
import Header.Root.View
import Header.Customer.View
import Header.Client.View
import Header.Site.View
import Header.Staff.View
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Components.Form as Form
import Ui.Helpers.Env
import Json.Decode as Decode


view : AuthToken -> Container -> Html Msg
view token container =
    div [ class "body-header" ]
        (viewWebData container.headerData (header token container.headerUi) headerPending)


headerPending : String -> List (Html Msg)
headerPending iconClass =
    [ div [ class "body-header-image" ] []
    , div [ class "body-header-extra header-loading" ]
        [ i [ class iconClass ] [] ]
    ]


header : AuthToken -> HeaderUi -> HeaderData -> List (Html Msg)
header token ui data =
    [ headerImage data.header
    , headerContent token data.header data.useraccess ui
    , div [ class "body-header-extra" ]
        [ text "Extra" ]
    ]


headerImage : Header -> Html Msg
headerImage header =
    let
        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"

        backgroundStyle image =
            ( "background-image", "url('" ++ endpoint ++ image ++ "')" )

        backgroundImage =
            (case header of
                RootHeader root ->
                    Maybe.map backgroundStyle root.values.image

                CustomerHeader customer ->
                    Maybe.map backgroundStyle customer.values.image

                ClientHeader client ->
                    Maybe.map backgroundStyle client.values.image

                SiteHeader site ->
                    Maybe.map backgroundStyle site.values.image

                StaffHeader staff ->
                    Maybe.map backgroundStyle staff.values.image

                Empty ->
                    Maybe.Nothing
            )
                |> Maybe.withDefault ( "display", "none" )
    in
        div
            [ class "body-header-image"
            , style [ backgroundImage ]
            ]
            []


dropdownMenuItem : AuthToken -> String -> String -> ModalType -> Html Msg
dropdownMenuItem token icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction token type_ Open) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


actionDropdownViewModel : AuthToken -> Header -> UserAccess -> HeaderUi -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel token header useraccess ui =
    let
        actions =
            [ ( "android-download", "Edit", EditHeader )
            , ( "trash-b", "Delete", DeleteHeader )
            ]

        actionFilter ( _, _, type_ ) =
            case type_ of
                EditHeader ->
                    useraccess.admin || useraccess.owner

                DeleteHeader ->
                    case header of
                        CustomerHeader _ ->
                            useraccess.admin && useraccess.root

                        _ ->
                            useraccess.admin

        accessibleActions =
            List.filter actionFilter actions
    in
        { element =
            Ui.IconButton.secondary "Actions"
                "chevron-down"
                "right"
                NoAction
        , items =
            List.map (\( icon, name, type_ ) -> dropdownMenuItem token icon name type_) accessibleActions
        }


headerActions : AuthToken -> Header -> UserAccess -> HeaderUi -> List (Html Msg)
headerActions token header useraccess ui =
    let
        dropdownViewModel =
            actionDropdownViewModel token header useraccess ui

        modalContent =
            case ui.editForm of
                Just form ->
                    [ Form.view EditFormMsg form ]

                Nothing ->
                    [ text "Edit Modal" ]

        editModalViewModel =
            { content = modalContent
            , title = "Edit Details"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Save" (ModalAction token EditHeader Save)
                    , Ui.Button.secondary "Cancel" (ModalAction token EditHeader Cancel)
                    ]
                ]
            }

        deleteModalViewModel =
            { content = [ text "Delete Modal" ]
            , title = "Delete Details"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Delete" (ModalAction token DeleteHeader Save)
                    , Ui.Button.secondary "Cancel" (ModalAction token DeleteHeader Cancel)
                    ]
                ]
            }
    in
        [ Ui.DropdownMenu.view dropdownViewModel ActionMenu ui.actionMenu
        , Ui.Modal.view (ModalMsg EditHeader) editModalViewModel ui.editModal
        , Ui.Modal.view (ModalMsg DeleteHeader) deleteModalViewModel ui.deleteModal
        ]


headerContent : AuthToken -> Header -> UserAccess -> HeaderUi -> Html Msg
headerContent token header useraccess ui =
    let
        headerItems =
            case header of
                RootHeader root ->
                    Header.Root.View.headerItems root

                CustomerHeader customer ->
                    Header.Customer.View.headerItems customer

                ClientHeader client ->
                    Header.Client.View.headerItems client

                SiteHeader site ->
                    Header.Site.View.headerItems site

                StaffHeader staff ->
                    Header.Staff.View.headerItems staff

                Empty ->
                    []

        headerContent =
            List.append
                headerItems
                (headerActions token header useraccess ui)
    in
        div [ class "body-header-content" ]
            headerContent


headerEmpty : List (Html Msg)
headerEmpty =
    [ text "Empty"
    ]
