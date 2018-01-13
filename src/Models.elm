module Models exposing (..)
import Window exposing (Size)
import Common exposing (prompt)
type alias Model =
    { route : Route
    , history : List (Maybe Command)
    , currentInput : String
    , windowSize : Size
    }

type alias Command =
  { cmd: String
  , out: Maybe String
  }

initialModel : Route -> Model
initialModel route =
    { route = route
    , history = []
    , currentInput = ""
    , windowSize = {height = 0, width = 0}
    }

type Route
    = HomeRoute
    | Speakers
    | Venue
    | NotFoundRoute
