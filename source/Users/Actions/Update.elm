module Users.Actions.Update exposing (..)

import Users.Actions.Models exposing (..)
import Helpers.Return as Return exposing (..)
import Helpers.Models exposing (..)
import Container.Out exposing (..)
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


update : Msg -> Model -> ReturnOut Msg OutMsg Model
update msg model =
    case ( model, msg ) of
        ( _, Open modalType user ) ->
            updateOpen model modalType user

        ( EditModel model_, EditMsg msg_ ) ->
            Edit.update msg_ model_
                |> mapBoth EditMsg EditModel

        ( RestrictModel model_, RestrictMsg msg_ ) ->
            Restrict.update msg_ model_
                |> mapBoth RestrictMsg RestrictModel

        ( ResetPasswordModel model_, ResetPasswordMsg msg_ ) ->
            ResetPassword.update msg_ model_
                |> mapBoth ResetPasswordMsg ResetPasswordModel

        ( ChangePasswordModel model_, ChangePasswordMsg msg_ ) ->
            ChangePassword.update msg_ model_
                |> mapBoth ChangePasswordMsg ChangePasswordModel

        ( DeleteModel model_, DeleteMsg msg_ ) ->
            Delete.update msg_ model_
                |> mapBoth DeleteMsg DeleteModel

        x ->
            logStray x model


updateOpen : Model -> ModalType -> User -> ReturnOut Msg OutMsg Model
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
