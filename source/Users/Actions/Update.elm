module Users.Actions.Update exposing (..)

import Users.Actions.Models exposing (..)
import Return exposing (..)
import Helpers.Models exposing (..)
import Users.Actions.Out exposing (..)
import Users.User exposing (..)
import Users.Edit.Models as EditModels
import Users.Restrict.Models as RestrictModels
import Users.ResetPassword.Models as ResetPasswordModels
import Users.ChangePassword.Models as ChangePasswordModels
import Users.Delete.Models as DeleteModels
import Users.Edit.Update as Edit
import Users.Restrict.Update as Restrict
import Users.ResetPassword.Update as ResetPassword
import Users.ChangePassword.Update as ChangePassword
import Users.Delete.Update as Delete


update : Msg -> Model -> ( Return Msg Model, OutMsg )
update msg model =
    let
        mapBothEx msg cmd ( return, out ) =
            ( Return.mapBoth msg cmd return, out )
    in
        case ( model, msg ) of
            ( _, Open modalType user ) ->
                ( updateOpen model modalType user, OutNone )

            ( EditModel model_, EditMsg msg_ ) ->
                Edit.update msg_ model_
                    |> mapBothEx EditMsg EditModel

            ( RestrictModel model_, RestrictMsg msg_ ) ->
                Restrict.update msg_ model_
                    |> mapBothEx RestrictMsg RestrictModel

            ( ResetPasswordModel model_, ResetPasswordMsg msg_ ) ->
                ResetPassword.update msg_ model_
                    |> mapBothEx ResetPasswordMsg ResetPasswordModel

            ( ChangePasswordModel model_, ChangePasswordMsg msg_ ) ->
                ChangePassword.update msg_ model_
                    |> mapBothEx ChangePasswordMsg ChangePasswordModel

            ( DeleteModel model_, DeleteMsg msg_ ) ->
                Delete.update msg_ model_
                    |> mapBothEx DeleteMsg DeleteModel

            x ->
                let
                    _ =
                        Debug.log "Stray found" x
                in
                    ( singleton model, OutNone )


updateOpen : Model -> ModalType -> User -> Return Msg Model
updateOpen model modalType user =
    case modalType of
        NewUser ->
            singleton
                (EditModel (EditModels.init user Post))

        EditUser ->
            singleton
                (EditModel (EditModels.init user Put))

        RestrictUser ->
            RestrictModels.init user
                |> mapBoth RestrictMsg RestrictModel

        ResetPasswordUser ->
            singleton (ResetPasswordModel (ResetPasswordModels.init user))

        ChangePasswordUser ->
            singleton (ChangePasswordModel (ChangePasswordModels.init (initChangePassword user.id)))

        DeleteUser ->
            singleton (DeleteModel (DeleteModels.init user))
