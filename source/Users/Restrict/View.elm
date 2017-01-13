module Users.Restrict.View exposing (..)

import Users.Restrict.Models exposing (..)
import Helpers.Models exposing (..)
import Helpers.RemoteData
import Return exposing (..)
import RemoteData exposing (..)
import Table exposing (..)
import Ui.Button
import Ui.Container
import Ui.Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict


view : AuthToken -> Model -> Html Msg
view token model =
    let
        modalContent =
            Helpers.RemoteData.view
                model.restrictions
                (viewModalContent model)
                (Helpers.RemoteData.viewPendingDefault "flexer")

        modalViewModel =
            { content = [ modalContent ]
            , title = "Edit Restrictions"
            , footer =
                [ Ui.Container.rowEnd []
                    [ Ui.Button.primary "Save" (Save token)
                    , Ui.Button.secondary "Cancel" Cancel
                    ]
                ]
            }
    in
        Ui.Modal.view ModalMsg modalViewModel model.modal


viewModalContent : Model -> List Restriction -> Html Msg
viewModalContent model restrictions =
    let
        lowerQuery =
            String.toLower model.query

        acceptableRestrictions =
            List.filter (String.contains lowerQuery << String.toLower << .name) restrictions
    in
        div [ class "modal-table" ]
            [ input [ placeholder "Search by Name", onInput SetQuery ] []
            , Table.view config model.tableState acceptableRestrictions
            ]



-- TABLE CONFIGURATION


config : Table.Config Restriction Msg
config =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ checkboxColumn "select" .selected .id
            , stringColumn "name" .name
            ]
        , customizations = customizations
        }


checkboxColumn : String -> (data -> Bool) -> (data -> NodeId) -> Table.Column data Msg
checkboxColumn name toCheck toId =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewCheckbox (toCheck data) (toId data)
        , sorter = Table.unsortable
        }


viewCheckbox : Bool -> NodeId -> Table.HtmlDetails Msg
viewCheckbox selected nodeId =
    Table.HtmlDetails [ class "checkbox" ]
        [ input [ type_ "checkbox", checked selected, onClick (ToggleRestriction nodeId) ] []
        ]


customizations : Table.Customizations Restriction Msg
customizations =
    let
        columnInfo =
            Dict.fromList
                [ ( "select", ( "", "checkbox" ) )
                , ( "name", ( "Name", "name" ) )
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
