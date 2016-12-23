module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = HomeRoute
    | CustomerRoute String
    | ClientRoute String
    | StaffRoute String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute (s "Home")
        , map CustomerRoute (s "Customer" </> string)
        , map ClientRoute (s "Client" </> string)
        , map StaffRoute (s "Staff" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
