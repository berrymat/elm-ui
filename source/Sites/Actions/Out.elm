module Sites.Actions.Out exposing (..)

import Sites.Site exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Site
    | OutDelete Site
    | OutNone
