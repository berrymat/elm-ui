module Clients.Actions.Models exposing (..)

import Clients.Edit.Models as Edit
import Clients.Delete.Models as Delete
import Clients.Client exposing (..)


type ModalType
    = NewClient
    | EditClient
    | DeleteClient


type Model
    = EditModel Edit.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType Client
    | EditMsg Edit.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
