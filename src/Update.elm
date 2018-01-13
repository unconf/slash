module Update exposing (..)

import Msgs exposing (Msg)
import Models exposing (..)
import Routing exposing (parseLocation, getRoute, navigate)
import Common exposing (prompt)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnLocationChange location ->
          ( { model | route = parseLocation location }, Cmd.none )
        Msgs.OnResize sz ->
          ( {model | windowSize = sz}, Cmd.none )
        Msgs.OnTextChanged s ->
            s
            |> extractInput
            |> processInput model

type Input =
  Completed String
  | Partial String

stripPrompt = String.dropLeft (String.length prompt)

extractInput : String -> Input
extractInput s =
  case String.lines s |> List.reverse of
    last :: prev :: rest ->
      if String.isEmpty last
        then
          prev
            |> stripPrompt
            |> Completed
        else
          last
            |> stripPrompt
            |> Partial
    _ -> Debug.crash "should never happen"

execute : String -> (Maybe Command, Cmd Msg)
execute s =
  case String.trim s |> String.words |> List.filter (String.isEmpty >> not) |> Debug.log "cmds" of
    [] -> (Nothing, Cmd.none)
    cmd :: params ->
      case executeImpl cmd params of
        Err err -> (Just (Command s (Just err)), Cmd.none)
        Ok (out, extCmd) -> (Just (Command s out), extCmd)

zeroOrOneParams : List a -> Result String (Maybe a)
zeroOrOneParams params =
  case params of
    [] -> Ok Nothing
    [dir] -> Ok (Just dir)
    _ -> Err "Too many parameters"

executeImpl : String -> List String -> Result String (Maybe String, Cmd Msg)
executeImpl cmd params =
  let paramToRoute : Maybe String -> Result String Route
      paramToRoute = Maybe.map getRoute
                     >> Maybe.withDefault (Result.Ok HomeRoute)
  in
    case cmd of
      "cd" ->
        zeroOrOneParams params
        |> Result.andThen paramToRoute
        |> Result.map (\ route -> (Nothing, navigate route))
      _ -> Err ("Unknown command: '" ++ cmd ++ "'")

processInput : Model -> Input -> (Model, Cmd Msg)
processInput m input =
  case input of
    Completed s ->
      let (cmd, extCmd) = execute s
      in ({ m | currentInput = "", history = cmd :: m.history}, extCmd)
    Partial s ->
      ( {m  | currentInput = s}, Cmd.none)
