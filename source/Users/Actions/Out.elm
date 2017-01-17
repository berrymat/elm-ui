module Users.Actions.Out exposing (..)

import Users.User exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate User
    | OutDelete User
    | OutNone
