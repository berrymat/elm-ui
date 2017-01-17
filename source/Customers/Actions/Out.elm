module Customers.Actions.Out exposing (..)

import Customers.Customer exposing (..)


type OutMsg
    = OutCancel
    | OutUpdate Customer
    | OutDelete Customer
    | OutNone
