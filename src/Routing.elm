module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Route(..))
import UrlParser exposing (..)
import Dict

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map Speakers (s "speakers")
        , map Venue (s "venue")
        ]

routeNames : List (Route, String)
routeNames =
  [ (HomeRoute, "")
  , (Speakers, "speakers")
  , (Venue, "venue")
  ]

mkPath : Route -> String
mkPath route =
  "/#" ++ ( routeNames
            |> List.filter (Tuple.first >> (==) route)
            |> List.map Tuple.second
            |> List.head
            |> Maybe.withDefault "notfound")

getRoute : String -> Result String Route
getRoute dir =
  routeNames
  |> List.filter (Tuple.second >> (==) dir)
  |> List.map Tuple.first
  |> List.head
  |> Result.fromMaybe "No such file or directory"

parseLocation : Location -> Route
parseLocation location =
    parseHash matchers location
    |> Maybe.withDefault NotFoundRoute

navigate : Route -> Cmd a
navigate = mkPath >> Navigation.modifyUrl
