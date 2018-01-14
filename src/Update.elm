module Update exposing (..)

import Msgs exposing (Msg)
import Models exposing (..)
import Routing exposing (parseLocation, getRoute, navigate)
import Task
import Dom
import Dom.Scroll

batchCmds : Cmd a -> Cmd a -> Cmd a
batchCmds a b = Cmd.batch [a, b]

tuple : a -> b -> (a, b)
tuple a b = (a, b)

noCmd : Model -> (Model, Cmd Msg)
noCmd m = (m, Cmd.none)

scrollMain : Cmd Msg
scrollMain =
  Dom.Scroll.toBottom "main"
  |> Task.attempt Msgs.OnTaskResult

focusShell : Cmd Msg
focusShell =
  Dom.focus "shell"
  |> Task.attempt Msgs.OnTaskResult

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnLocationChange location ->
          noCmd { model | route = parseLocation location }
        Msgs.OnResize sz ->
          noCmd {model | windowSize = sz}
        Msgs.OnTextChanged s -> noCmd {model | currentInput = s}
        Msgs.OnEnter ->
          model.currentInput
          |> processInput model
          |> Tuple.mapSecond (batchCmds scrollMain)
        Msgs.OnRefocus -> (model, focusShell)
        Msgs.OnTaskResult r -> noCmd model


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

processInput : Model -> String -> (Model, Cmd Msg)
processInput m input =
      let (cmd, extCmd) = execute input
      in ({ m | currentInput = "", history = cmd :: m.history}, extCmd)
