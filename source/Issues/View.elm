module Issues.View exposing (..)

import Issues.Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Table exposing (..)
import Helpers.Models exposing (..)
import Dict
import Ui
import Ui.DropdownMenu
import Ui.IconButton
import Issues.Issue exposing (..)
import Issues.Actions.View as Actions
import Issues.Actions.Models exposing (ModalType(..))
import Helpers.Table exposing (..)
import Ui.Helpers.Env
import Json.Decode as Decode


view : AuthToken -> Model -> Html Msg
view token model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableIssues =
            List.filter (String.contains lowerQuery << String.toLower << .caseNumber) model.issues
    in
        div [ class "body-content" ]
            [ div [ class "body-content-content" ]
                [ div [ class "body-content-content-content" ]
                    [ div [ class "file-content" ]
                        [ input [ placeholder "Search by Name", onInput SetQuery ] []
                        , Table.view config model.tableState acceptableIssues
                        ]
                    ]
                , div [ class "body-content-content-footer" ]
                    [ viewActionMenu token model
                    , div [ class "flexer" ] []
                    ]
                ]
            ]


notesColumn : String -> (data -> List String) -> Table.Column data Msg
notesColumn name toNotes =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewNotes (toNotes data)
        , sorter = Table.unsortable
        }


viewNotes : List String -> Table.HtmlDetails Msg
viewNotes notes =
    Table.HtmlDetails [ class "notes" ]
        (List.map (\n -> p [ class "note" ] [ text n ]) notes)


photoColumn : String -> (data -> String) -> Table.Column data Msg
photoColumn name toUrl =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewPhoto (toUrl data)
        , sorter = Table.unsortable
        }


viewPhoto : String -> Table.HtmlDetails Msg
viewPhoto url =
    let
        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"

        fullUrl =
            endpoint ++ url

        backgroundStyle url =
            if (String.isEmpty url) then
                ( "", "" )
            else
                ( "background-image", "url('" ++ fullUrl ++ "')" )
    in
        Table.HtmlDetails [ class "photo" ]
            [ if (String.isEmpty url) then
                (text "")
              else
                (img [ src fullUrl ] [])
            ]


config : Table.Config Issue Msg
config =
    let
        columnInfo =
            Dict.fromList
                [ ( "select", ( "", "radio" ) )
                , ( "caseNumber", ( "Case Number", "caseNumber" ) )
                , ( "createdDateTime", ( "Created", "date" ) )
                , ( "closedDateTime", ( "Closed", "date" ) )
                , ( "status", ( "Status", "status" ) )
                , ( "siteName", ( "Site", "siteName" ) )
                , ( "photoUrl", ( "Photo", "photoUrl" ) )
                , ( "notes", ( "Notes", "notes" ) )
                ]
    in
        Table.customConfig
            { toId = .id
            , toMsg = SetTableState
            , columns =
                [ radioColumn "select" .checked .id ToggleIssue
                , stringColumn "caseNumber" .caseNumber
                , datetimeColumn "createdDateTime" .createdDateTime
                , datetimeColumn "closedDateTime" .closedDateTime
                , stringColumn "status" .status
                , stringColumn "siteName" .siteName
                , photoColumn "photoUrl" .photoUrl
                , notesColumn "notes" .notes
                ]
            , customizations = (customizations columnInfo)
            }



-- ACTION DROPDOWN


dropdownMenuItem : AuthToken -> String -> String -> ModalType -> Html Msg
dropdownMenuItem token icon name type_ =
    Ui.DropdownMenu.item [ onClick (ModalAction token type_) ]
        [ Ui.icon icon True []
        , node "span" [] [ text name ]
        ]


accessibleActions : Model -> List ( String, String, ModalType )
accessibleActions model =
    let
        issue =
            List.filter .checked model.issues
                |> List.head

        actions =
            [ ( "plus", "New Case", NewIssue )
            , ( "record", "Add Comment", EditIssue )
            ]

        hasIssueAnd can =
            Maybe.map (\_ -> can) issue
                |> Maybe.withDefault False

        actionFilter ( _, _, type_ ) =
            case type_ of
                NewIssue ->
                    model.canAdd

                EditIssue ->
                    hasIssueAnd model.canEdit
    in
        List.filter actionFilter actions


actionDropdownViewModel : List ( String, String, ModalType ) -> AuthToken -> Ui.DropdownMenu.ViewModel Msg
actionDropdownViewModel actions token =
    { element =
        Ui.IconButton.secondary "Issue Actions"
            "chevron-up"
            "right"
            NoAction
    , items =
        List.map (\( icon, name, type_ ) -> dropdownMenuItem token icon name type_) actions
    }


viewActionMenu : AuthToken -> Model -> Html Msg
viewActionMenu token model =
    let
        actions =
            accessibleActions model

        issue =
            List.filter .checked model.issues
                |> List.head
                |> Maybe.withDefault (initIssue model.id)
    in
        case actions of
            [] ->
                div [] []

            _ ->
                div []
                    [ Ui.DropdownMenu.view (actionDropdownViewModel actions token)
                        ActionMenu
                        model.actionMenu
                    , Html.map ActionsMsg (Actions.view token model.actions)
                    ]
