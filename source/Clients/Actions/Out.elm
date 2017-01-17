module Clients.Actions.Out exposing (..)

import Clients.Client exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Client
    | OutDelete Client
    | OutNone
