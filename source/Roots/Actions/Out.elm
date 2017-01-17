module Roots.Actions.Out exposing (..)

import Roots.Root exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Root
    | OutDelete Root
    | OutNone
