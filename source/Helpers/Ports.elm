port module Helpers.Ports exposing (..)

-- port for sending strings out to JavaScript


port download : { url : String, files : List String } -> Cmd msg



-- port for listening for suggestions from JavaScript


port downloadResponse : (String -> msg) -> Sub msg
