module Reset.Update exposing (..)

import Reset.Models exposing (..)
import Return exposing (..)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        LoadReset ->
            singleton model

        AuthenticateResponse webdata ->
            singleton model

        LoadToken ->
            singleton model
