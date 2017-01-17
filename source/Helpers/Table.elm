module Helpers.Table exposing (..)

import Dict exposing (..)
import Table
import Helpers.Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date
import Date.Extra.Config.Config_en_us as Config
import Date.Extra.Format as Format exposing (format)


radioColumn : String -> (data -> Bool) -> (data -> NodeId) -> (String -> msg) -> Table.Column data msg
radioColumn name toCheck toId toggleMsg =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewRadio (toCheck data) (toId data) toggleMsg
        , sorter = Table.unsortable
        }


viewRadio : Bool -> NodeId -> (String -> msg) -> Table.HtmlDetails msg
viewRadio selected nodeId toggleMsg =
    Table.HtmlDetails [ class "radio" ]
        [ input [ type_ "radio", checked selected, onClick (toggleMsg nodeId) ] []
        ]


checkColumn : String -> (data -> Bool) -> (data -> NodeId) -> (String -> msg) -> Table.Column data msg
checkColumn name toCheck toId toggleMsg =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewCheck (toCheck data) (toId data) toggleMsg
        , sorter = Table.unsortable
        }


viewCheck : Bool -> NodeId -> (String -> msg) -> Table.HtmlDetails msg
viewCheck selected nodeId toggleMsg =
    Table.HtmlDetails [ class "checkbox" ]
        [ input [ type_ "checkbox", checked selected, onClick (toggleMsg nodeId) ] []
        ]


datetimeColumn : String -> (data -> Float) -> Table.Column data msg
datetimeColumn name toDatetime =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewDatetime (toDatetime data)
        , sorter = Table.increasingOrDecreasingBy toDatetime
        }


viewDatetime : Float -> Table.HtmlDetails msg
viewDatetime datetime =
    let
        date =
            Date.fromTime datetime

        cfg =
            Config.config
    in
        Table.HtmlDetails [ class "datetime" ]
            [ text
                (if datetime == 0.0 then
                    ""
                 else
                    (format cfg cfg.format.dateTime date)
                )
            ]


customizations : Dict String ( String, String ) -> Table.Customizations item msg
customizations columnInfo =
    let
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
