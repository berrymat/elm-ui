module Users.Actions.Models exposing (..)

import Users.Edit.Models as Edit
import Users.Restrict.Models as Restrict
import Users.ResetPassword.Models as ResetPassword
import Users.ChangePassword.Models as ChangePassword
import Users.Delete.Models as Delete
import Users.User exposing (..)


type ModalType
    = NewUser
    | EditUser
    | RestrictUser
    | ResetPasswordUser
    | ChangePasswordUser
    | DeleteUser


type Model
    = EditModel Edit.Model
    | RestrictModel Restrict.Model
    | ResetPasswordModel ResetPassword.Model
    | ChangePasswordModel ChangePassword.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType User
    | EditMsg Edit.Msg
    | RestrictMsg Restrict.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | ChangePasswordMsg ChangePassword.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
