module Sites.Actions.Models exposing (..)

import Sites.Edit.Models as Edit
import Sites.Delete.Models as Delete
import Sites.Site exposing (..)


type ModalType
    = NewSite
    | EditSite
    | DeleteSite


type Model
    = EditModel Edit.Model
    | DeleteModel Delete.Model
    | NoModel


type Msg
    = Open ModalType Site
    | EditMsg Edit.Msg
    | DeleteMsg Delete.Msg


init : Model
init =
    NoModel
