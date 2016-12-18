module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = ContainerRoot
    | ContainerRoute String String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map ContainerRoute (s "container" </> string </> s "path" </> string)
        , map ContainerRoot (s "container")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
