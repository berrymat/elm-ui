module Users.Manager.Out exposing (..)

import Users.Manager.User exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate User
    | OutDelete User
    | OutNone
