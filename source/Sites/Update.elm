module Sites.Update exposing (..)

import Sites.Models exposing (..)
import Return exposing (..)


update : Msg -> Site -> Return Msg Site
update msg model =
    case msg of
        Todo ->
            singleton model
