module Staffs.Actions.Out exposing (..)

import Staffs.Staff exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Staff
    | OutDelete Staff
    | OutNone
