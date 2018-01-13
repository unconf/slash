module Main exposing (..)
import Models exposing (Model, initialModel)
import Msgs exposing (Msg(..))
import Navigation exposing (Location)
import Routing
import Update exposing (update)
import View exposing (view)
import Html.Styled exposing (toUnstyled)
import Window
import Task

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform Msgs.OnResize Window.size

init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        ( initialModel currentRoute, initialSizeCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      [ Window.resizes OnResize
      ]


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
