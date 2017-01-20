module Customers.Update exposing (..)

import Customers.Models exposing (..)
import Return exposing (..)


update : Msg -> Customer -> Return Msg Customer
update msg model =
    case msg of
        Todo ->
            singleton model
