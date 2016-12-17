module Header.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Container.Messages exposing (..)
import Container.Models exposing (..)
import Helpers.Models exposing (..)
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
import Components.Form as Form


view : Container -> Html Msg
view container =
    div [ class "body-header" ]
        (case container.headerData of
            NotAsked ->
                [ text "Initializing." ]

            Loading ->
                [ text "Loading." ]

            Failure err ->
                [ text ("Error: " ++ toString err) ]

            Success data ->
                (header data.header data.useraccess container.headerUi)
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


dropdownMenuItem : String -> String -> ModalType -> Html Msg
dropdownMenuItem icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction type_ Open) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


actionDropdownViewModel : Header -> UserAccess -> HeaderUi -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel header useraccess ui =
    let
        actions =
            [ ( "android-download", "Edit", Edit )
            , ( "trash-b", "Delete", Delete )
            ]

        actionFilter ( _, _, type_ ) =
            case type_ of
                Edit ->
                    useraccess.admin || useraccess.owner

                Delete ->
                    case header of
                        CustomerHeader _ ->
                            useraccess.admin && useraccess.root

                        _ ->
                            useraccess.admin

        {-
           _ ->
               useraccess.admin
        -}
        accessibleActions =
            List.filter actionFilter actions
    in
        { element =
            Ui.IconButton.secondary "Actions"
                "chevron-down"
                "right"
                NoAction
        , items =
            List.map (\( icon, name, type_ ) -> dropdownMenuItem icon name type_) accessibleActions
        }


headerActions : Header -> UserAccess -> HeaderUi -> List (Html Msg)
headerActions header useraccess ui =
    let
        dropdownViewModel =
            actionDropdownViewModel header useraccess ui

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
                    [ Ui.Button.primary "Save" (ModalAction Edit Save)
                    , Ui.Button.secondary "Cancel" (ModalAction Edit Cancel)
                    ]
                ]
            }

        deleteModalViewModel =
            { content = [ text "Delete Modal" ]
            , title = "Delete Details"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Delete" (ModalAction Delete Save)
                    , Ui.Button.secondary "Cancel" (ModalAction Delete Cancel)
                    ]
                ]
            }
    in
        [ Ui.DropdownMenu.view dropdownViewModel ActionMenu ui.actionMenu
        , Ui.Modal.view (ModalMsg Edit) editModalViewModel ui.editModal
        , Ui.Modal.view (ModalMsg Delete) deleteModalViewModel ui.deleteModal
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
