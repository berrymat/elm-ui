module Header.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Header.Models exposing (..)
import Helpers.Models exposing (..)
import Header.Models exposing (..)
import Roots.View
import Customers.View
import Clients.View
import Sites.View
import Staffs.View
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Components.Form as Form
import Ui.Helpers.Env
import Json.Decode as Decode


view : AuthToken -> Model -> Html Msg
view token model =
    div [ class "body-header" ]
        [ headerImage model.header
        , headerContent token model
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
                    Maybe.map backgroundStyle root.root.image

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



{-
   dropdownMenuItem : AuthToken -> String -> String -> ModalType -> Html Msg
   dropdownMenuItem token icon name type_ =
       Ui.DropdownMenu.item [ onClick (ModalAction token type_ Open) ]
           [ Ui.icon icon True []
           , node "span" [] [ text name ]
           ]


   actionDropdownViewModel : AuthToken -> Model -> Ui.DropdownMenu.ViewModel Msg
   actionDropdownViewModel token model =
       let
           actions =
               [ ( "android-download", "Edit", EditHeader )
               ]

           useraccess =
               model.useraccess

           actionFilter ( _, _, type_ ) =
               case type_ of
                   EditHeader ->
                       useraccess.admin || useraccess.owner

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


   headerActions : AuthToken -> Model -> List (Html Msg)
   headerActions token model =
       let
           dropdownViewModel =
               actionDropdownViewModel token model

           modalContent =
               case model.editForm of
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
       in
           [ Ui.DropdownMenu.view dropdownViewModel ActionMenu model.actionMenu
           , Ui.Modal.view (ModalMsg EditHeader) editModalViewModel model.editModal
           ]
-}


headerContent : AuthToken -> Model -> Html Msg
headerContent token model =
    let
        headerItems =
            case model.header of
                RootHeader root ->
                    Roots.View.headerItems root

                CustomerHeader customer ->
                    Customers.View.headerItems customer

                ClientHeader client ->
                    Clients.View.headerItems client

                SiteHeader site ->
                    Sites.View.headerItems site

                StaffHeader staff ->
                    Staffs.View.headerItems staff

                Empty ->
                    []
    in
        div [ class "body-header-content" ]
            headerItems


viewActions : AuthToken -> Model -> Html Msg
viewActions token model =
    case model.header of
        RootHeader root ->
            Html.map RootsMsg (Roots.View.viewActionMenu token root)

        CustomerHeader customer ->
            div [] [ text "Customer TODO" ]

        ClientHeader client ->
            div [] [ text "Client TODO" ]

        SiteHeader site ->
            div [] [ text "Site TODO" ]

        StaffHeader staff ->
            div [] [ text "Staff TODO" ]

        Empty ->
            div [] []
