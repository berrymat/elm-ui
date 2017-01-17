module Staffs.Actions.Models exposing (..)

import Staffs.Edit.Models as Edit
import Staffs.Delete.Models as Delete
import Staffs.Staff exposing (..)


type ModalType
    = NewStaff
    | EditStaff
    | DeleteStaff


type Model
    = EditModel Edit.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType Staff
    | EditMsg Edit.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
