module Content.Commands exposing (..)

import Json.Decode as Decode exposing (field, at)
import Content.Models exposing (..)
import Helpers.Models exposing (..)
import Helpers.Helpers exposing (..)


-- DECODERS


casesDecoder : Decode.Decoder Cases
casesDecoder =
    Decode.map2 Cases
        (field "id" Decode.string)
        (field "name" Decode.string)


casesUrl : NodeId -> String
casesUrl nodeId =
    apiUrl ++ "Cases/" ++ nodeId
