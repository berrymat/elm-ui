module Roots.Actions.Models exposing (..)

import Roots.Edit.Models as Edit
import Roots.Delete.Models as Delete
import Roots.Root exposing (..)


type ModalType
    = NewRoot
    | EditRoot
    | DeleteRoot


type Model
    = EditModel Edit.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType Root
    | EditMsg Edit.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
