module Users.View exposing (..)

import Users.Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers.Helpers exposing (..)
import Table exposing (..)
import Helpers.Models exposing (..)
import Dict
import Ui
import Ui.Button
import Ui.Container
import Ui.DropdownMenu
import Ui.IconButton
import Ui.Modal
import Components.Form as Form


view : Model -> AuthToken -> Html Msg
view model token =
    let
        lowerQuery =
            String.toLower model.query

        acceptableUsers =
            List.filter (String.contains lowerQuery << String.toLower << .email) model.users
    in
        div [ class "body-content" ]
            [ div [ class "body-content-content" ]
                [ div [ class "body-content-content-content" ]
                    [ div [ class "file-content" ]
                        [ input [ placeholder "Search by Email", onInput SetQuery ] []
                        , Table.view config model.tableState acceptableUsers
                        ]
                    ]
                , div [ class "body-content-content-footer" ]
                    [ viewActionMenu model token
                    , div [ class "flexer" ] []
                    ]
                ]
            ]


config : Table.Config User Msg
config =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ radioColumn "select" .checked .id
            , stringColumn "email" .email
            , stringColumn "firstName" .firstName
            , stringColumn "lastName" .lastName
            ]
        , customizations = customizations
        }


radioColumn : String -> (data -> Bool) -> (data -> NodeId) -> Table.Column data Msg
radioColumn name toCheck toId =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewRadio (toCheck data) (toId data)
        , sorter = Table.unsortable
        }


viewRadio : Bool -> NodeId -> Table.HtmlDetails Msg
viewRadio selected nodeId =
    Table.HtmlDetails [ class "radio" ]
        [ input [ type_ "radio", checked selected, onClick (ToggleUser nodeId) ] []
        ]


customizations : Table.Customizations User Msg
customizations =
    let
        columnInfo =
            Dict.fromList
                [ ( "select", ( "", "radio" ) )
                , ( "email", ( "Email", "email" ) )
                , ( "firstName", ( "First Name", "firstName" ) )
                , ( "lastName", ( "Last Name", "lastName" ) )
                ]

        simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
        simpleThead headers =
            Table.HtmlDetails [] (List.map simpleTheadHelp headers)

        simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
        simpleTheadHelp ( name, status, onClick ) =
            let
                ( caption, className ) =
                    (Dict.get name columnInfo) |> Maybe.withDefault ( "", "" )

                content =
                    case status of
                        Table.Unsortable ->
                            [ Html.text caption ]

                        Table.Sortable selected ->
                            [ Html.text caption
                            , if selected then
                                darkGrey "↓"
                              else
                                lightGrey "↓"
                            ]

                        Table.Reversible Nothing ->
                            [ Html.text caption
                            , lightGrey "↕"
                            ]

                        Table.Reversible (Just isReversed) ->
                            [ Html.text caption
                            , darkGrey
                                (if isReversed then
                                    "↑"
                                 else
                                    "↓"
                                )
                            ]
            in
                Html.th [ onClick, class className ] content
    in
        { tableAttrs = []
        , caption = Nothing
        , thead = simpleThead
        , tfoot = Nothing
        , tbodyAttrs = []
        , rowAttrs = simpleRowAttrs
        }


darkGrey : String -> Html msg
darkGrey symbol =
    Html.span [ style [ ( "color", "#555" ) ] ] [ Html.text (" " ++ symbol) ]


lightGrey : String -> Html msg
lightGrey symbol =
    Html.span [ style [ ( "color", "#ccc" ) ] ] [ Html.text (" " ++ symbol) ]


simpleRowAttrs : data -> List (Attribute msg)
simpleRowAttrs _ =
    []



-- ACTION DROPDOWN


dropdownMenuItem : String -> String -> ModalType -> AuthToken -> Html Msg
dropdownMenuItem icon name type_ token =
    Ui.DropdownMenu.item [ onClick (ModalAction type_ Open token) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


accessibleActions : Model -> List ( String, String, ModalType )
accessibleActions model =
    let
        user =
            List.filter .checked model.users
                |> List.head

        actions =
            [ ( "plus", "New User", NewUser )
            , ( "record", "Edit User", EditUser )
            , ( "record", "Restrictions", RestrictUser )
            , ( "record", "Reset Password", ResetPasswordUser )
            , ( "record", "Change Password", ChangePasswordUser )
            , ( "trash-b", "Delete User", DeleteUser )
            ]

        hasUserAnd can =
            Maybe.map (\_ -> can) user
                |> Maybe.withDefault False

        actionFilter ( _, _, type_ ) =
            case type_ of
                NewUser ->
                    model.canAdd

                EditUser ->
                    hasUserAnd model.canEdit

                RestrictUser ->
                    hasUserAnd model.canRestrict

                ResetPasswordUser ->
                    hasUserAnd model.canResetPassword

                ChangePasswordUser ->
                    hasUserAnd model.canChangePassword

                DeleteUser ->
                    hasUserAnd model.canDelete
    in
        List.filter actionFilter actions


actionDropdownViewModel : List ( String, String, ModalType ) -> AuthToken -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel actions token =
    { element =
        Ui.IconButton.secondary "User Actions"
            "chevron-up"
            "right"
            NoAction
    , items =
        List.map (\( icon, name, type_ ) -> dropdownMenuItem icon name type_ token) actions
    }


viewActionMenu : Model -> AuthToken -> Html Msg
viewActionMenu model token =
    let
        actions =
            accessibleActions model

        user =
            List.filter .checked model.users
                |> List.head
                |> Maybe.withDefault (initUser model.id)

        modalContent =
            case model.userEditForm of
                Just form ->
                    [ Form.view UserFormMsg form ]

                Nothing ->
                    [ text "Edit Modal" ]

        ( title, saveText, modalType ) =
            case model.userEditMethod of
                Just Post ->
                    ( "New User", "Create", NewUser )

                Just Put ->
                    ( "Edit User", "Update", EditUser )

                Nothing ->
                    ( "", "", NewUser )

        userEditModalViewModel =
            { content = modalContent
            , title = title
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary saveText (ModalAction modalType Save token)
                    , Ui.Button.secondary "Cancel" (ModalAction modalType Cancel token)
                    ]
                ]
            }

        userDeleteModalViewModel =
            { content =
                [ div [ class "padded-modal-content" ]
                    [ text ("Confirm deletion of user '" ++ user.email ++ "'?") ]
                ]
            , title = "Delete User"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.danger "Delete" (ModalAction DeleteUser Save token)
                    , Ui.Button.secondary "Cancel" (ModalAction DeleteUser Cancel token)
                    ]
                ]
            }
    in
        case actions of
            [] ->
                div [] []

            _ ->
                div []
                    [ Ui.DropdownMenu.view (actionDropdownViewModel actions token)
                        ActionMenu
                        model.usersActionMenu
                    , Ui.Modal.view (ModalMsg modalType) userEditModalViewModel model.userEditModal
                    , Ui.Modal.view (ModalMsg DeleteUser) userDeleteModalViewModel model.userDeleteModal
                    ]
