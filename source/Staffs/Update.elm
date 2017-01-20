module Staffs.Update exposing (..)

import Staffs.Models exposing (..)
import Return exposing (..)


update : Msg -> Staff -> Return Msg Staff
update msg model =
    case msg of
        Todo ->
            singleton model
