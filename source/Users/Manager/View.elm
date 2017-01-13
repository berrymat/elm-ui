module Users.Manager.View exposing (..)

import Users.Manager.Models exposing (..)
import Helpers.Models exposing (..)
import Html exposing (..)
import Users.Edit.View as Edit
import Users.Restrict.View as Restrict
import Users.ResetPassword.View as ResetPassword
import Users.ChangePassword.View as ChangePassword
import Users.Delete.View as Delete


view : AuthToken -> Model -> Html Msg
view token model =
    case model of
        EditModel subModel ->
            Html.map EditMsg (Edit.view token subModel)

        RestrictModel subModel ->
            Html.map RestrictMsg (Restrict.view token subModel)

        ResetPasswordModel subModel ->
            Html.map ResetPasswordMsg (ResetPassword.view token subModel)

        ChangePasswordModel subModel ->
            Html.map ChangePasswordMsg (ChangePassword.view token subModel)

        DeleteModel subModel ->
            Html.map DeleteMsg (Delete.view token subModel)

        NoModel ->
            div [] []
