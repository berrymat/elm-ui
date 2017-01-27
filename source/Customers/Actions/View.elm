module Customers.Actions.View exposing (..)

import Customers.Actions.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Customers.Edit.View
import Customers.Delete.View
import Clients.Edit.View
import Staffs.Edit.View


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditCustomerModel subModel ->
            Html.map EditCustomerMsg (Customers.Edit.View.view token subModel)

        DeleteCustomerModel subModel ->
            Html.map DeleteCustomerMsg (Customers.Delete.View.view token subModel)

        EditClientModel subModel ->
            Html.map EditClientMsg (Clients.Edit.View.view token subModel)

        EditStaffModel subModel ->
            Html.map EditStaffMsg (Staffs.Edit.View.view token subModel)

        NoModel ->
            div [] []
