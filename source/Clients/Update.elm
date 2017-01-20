module Clients.Update exposing (..)

import Clients.Models exposing (..)
import Return exposing (..)


update : Msg -> Client -> Return Msg Client
update msg model =
    case msg of
        Todo ->
            singleton model
