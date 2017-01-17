module Issues.Actions.Out exposing (..)

import Issues.Issue exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Issue
    | OutNone
