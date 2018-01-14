module View exposing (..)

import Html.Styled exposing (Html, main_, div, img, text, textarea, span, input, p)
import Html.Styled.Attributes exposing (css, value, src, class, id)
import Html.Styled.Events exposing (onInput)
import Css exposing (..)
import Models exposing (Model, Command)
import Msgs exposing (Msg(..))

black : Color
black = hex "181818"
white : Color
white = hex "d3d3d3"
blue : Color
blue = hex "AE81FF"
green : Color
green = hex "33FF33"

view : Model -> Html Msg
view model =
  div [css
        [ color white
        , backgroundColor black
        , model.windowSize.height |> toFloat |> px |> height
        ]]
      [ div [ class "container"
            , css [ backgroundColor black
                  , color white]]
            ( greetings
            ++ renderHistory model.history
            ++ commandInput model)]

maybeToList : Maybe a -> List a
maybeToList mv =
  case mv of
    Nothing -> []
    Just x -> [x]

renderHistory : List (Maybe Command) -> List (Html Msg)
renderHistory history =
  let renderCmd maybeCmd =
        div []
          ( [ div [] [span []
                      [ text prompt]
                      , text ( maybeCmd
                                |> Maybe.map (\c -> c.cmd)
                                |> Maybe.withDefault "")]
            ] ++ ( maybeCmd
                    |> Maybe.andThen (\c -> c.out |> Maybe.map text)
                    |> maybeToList))
  in
    history
    |> List.reverse
    |> List.map renderCmd

prompt : String
prompt = "[www@slash.unconf.ro]~>"

commandInput : Model -> List(Html Msg)
commandInput model =
    [ div [css [ color blue
                -- , backgroundColor Css.Colors.red
                , float left
                , width (px 200)]]
           [text prompt]
    , div []
          [ input
            [ id "shell"
            , css [ color white
                  , backgroundColor black
                  , borderStyle none
                  , outline none
                  , overflow hidden
                  , width (px 300)]
            , onInput OnTextChanged]
            []
          ]
    ]

-- <img src="logo.svg" alt="Smiley face" height="260" width="260">
-- <p>***  Welcome to slash unconf  ***</p>
-- <p>***  THE MOST CONFORTABLE UNCONFERENCE ON FUNCTIONAL PROGRAMMING   ***</p>
-- <p>***  (start with "<span class="green">help</span>" to find your way)   ***</p>
greetings : List (Html Msg)
greetings =
  [ img [src "logo.svg"
        , css [height (px 260), width (px 260)]
        ] []
  , p [] [text "***  Welcome to slash unconf  ***"]
  , p [] [text "***  THE MOST CONFORTABLE UNCONFERENCE ON FUNCTIONAL PROGRAMMING   ***"]
  , p [] [ text "***  (start with \""
         , span [css [color green]] [text "help\""]
         , text " to find your way)   ***"
         ]
  ]
