module Issues.Actions.Models exposing (..)

import Issues.Edit.Models as Edit
import Issues.Issue exposing (..)


type ModalType
    = NewIssue
    | EditIssue


type Model
    = EditModel Edit.Model
    | NoModel


type Msg
    = Open ModalType (List IssueSite) Issue
    | EditMsg Edit.Msg


init : Model
init =
    NoModel
