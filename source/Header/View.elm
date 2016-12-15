module Header.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Container.Messages exposing (..)
import Header.Models exposing (..)
import Header.Root.View
import Header.Customer.View
import Header.Client.View
import Header.Site.View
import Header.Staff.View
import RemoteData exposing (..)
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal


view : HeaderInfo -> Html Msg
view headerInfo =
    div [ class "body-header" ]
        (case headerInfo.data of
            NotAsked ->
                [ text "Initializing." ]

            Loading ->
                [ text "Loading." ]

            Failure err ->
                [ text ("Error: " ++ toString err) ]

            Success data ->
                (header data.header data.useraccess headerInfo.ui)
        )


header : Header -> UserAccess -> HeaderUi -> List (Html Msg)
header header useraccess ui =
    [ headerImage header
    , headerContent header useraccess ui
    , div [ class "body-header-extra" ]
        [ text "Extra" ]
    ]


headerImage : Header -> Html Msg
headerImage header =
    let
        backgroundStyle image =
            ( "background-image", "url('" ++ image ++ "')" )

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


actionDropdownViewModel : Header -> UserAccess -> HeaderUi -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel header useraccess ui =
    { element =
        Ui.IconButton.secondary "Actions"
            "chevron-down"
            "right"
            NoAction
    , items =
        [ Ui.DropdownMenu.item [ onClick OpenEditModal ]
            [ Ui.icon "android-download" True []
            , node "span" [] [ text "Edit" ]
            ]
        , Ui.DropdownMenu.item [ onClick OpenDeleteModal ]
            [ Ui.icon "trash-b" True []
            , node "span" [] [ text "Delete" ]
            ]
        ]
    }


headerActions : Header -> UserAccess -> HeaderUi -> List (Html Msg)
headerActions header useraccess ui =
    let
        dropdownViewModel =
            actionDropdownViewModel header useraccess ui

        editModalViewModel =
            { content = [ text "Edit Modal" ]
            , title = "Edit Details"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Save" CloseEditModal
                    , Ui.Button.secondary "Close" CloseEditModal
                    ]
                ]
            }

        deleteModalViewModel =
            { content = [ text "Delete Modal" ]
            , title = "Delete Details"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Save" CloseDeleteModal
                    , Ui.Button.secondary "Close" CloseDeleteModal
                    ]
                ]
            }
    in
        [ Ui.DropdownMenu.view dropdownViewModel ActionMenu ui.actionMenu
        , Ui.Modal.view EditModal editModalViewModel ui.editModal
        , Ui.Modal.view DeleteModal deleteModalViewModel ui.deleteModal
        ]


headerContent : Header -> UserAccess -> HeaderUi -> Html Msg
headerContent header useraccess ui =
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
                (headerActions header useraccess ui)
    in
        div [ class "body-header-content" ]
            headerContent


headerEmpty : List (Html Msg)
headerEmpty =
    [ text "Empty"
    ]
