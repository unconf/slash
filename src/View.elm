module View exposing (..)

import Html.Styled exposing (Html, div, img, text, textarea)
import Html.Styled.Attributes exposing (css, value, src)
import Html.Styled.Events exposing (onInput)
import Css exposing (..)
import Css.Colors
import Models exposing (Model, Command)
import Msgs exposing (Msg(..))
import Window exposing (Size)
import Common exposing (prompt)

view : Model -> Html Msg
view model =
    div [css [ color Css.Colors.lime
             , backgroundColor Css.Colors.black
             , height (toFloat model.windowSize.height |> px)
             , width (toFloat model.windowSize.width |> px)]]
        [ page model ]


page : Model -> Html Msg
page model =
  div []
      [ banner model
      , shell model
      ]

renderText : List (Maybe Command) -> String -> String
renderText history currentInput =
  let renderCmd maybeCmd =
        "\n" ++ prompt ++
          (maybeCmd
           |> Maybe.map renderOutput
           |> Maybe.withDefault "")
      renderOutput c =
        c.cmd ++ (case c.out of
                    Just out -> "\n" ++ out
                    Nothing -> "")
  in
    "Enter a command:"
      ++ ( history
           |> List.reverse
           |> List.map renderCmd
           |> String.concat)
      ++ "\n" ++ prompt ++ currentInput

shell : Model -> Html Msg
shell model =
  textarea
    [ value (renderText model.history model.currentInput)
    , onInput OnTextChanged
    , shellCss model.windowSize]
    []

shellCss : Size -> Html.Styled.Attribute Msg
shellCss windowSize =
  css
    [ color Css.Colors.lime
    , backgroundColor Css.Colors.black
    , (toFloat windowSize.height / 2)  - 20 |> px |> height
    , (windowSize.width - 20)|> toFloat |> px |> width
    ]

banner : Model -> Html Msg
banner model =
    case model.route of
        Models.HomeRoute -> homePage model
        Models.Speakers -> speakersPage model
        Models.Venue -> venuePage model
        Models.NotFoundRoute -> notFoundPage


notFoundPage : Html msg
notFoundPage =
    div []
        [ text "Not found"
        ]

homePage : Model -> Html msg
homePage model =
    div [css [ toFloat model.windowSize.height / 2 |> px |> height
             , model.windowSize.width |> toFloat |> px |> width
             , color Css.Colors.lime
             , backgroundColor Css.Colors.black]]
        [ img [src "/logo.png"] []
        , text "Home sweet home"
        ]

speakersPage : Model -> Html msg
speakersPage model =
    div [css [ toFloat model.windowSize.height / 2  |> px |> height
             , model.windowSize.width |> toFloat |> px |> width
             , color Css.Colors.lime
             , backgroundColor Css.Colors.black]]
        [ img [src "/logo.png"] []
        , text "Speakers"
        ]

venuePage : Model -> Html msg
venuePage model =
    div [css [ toFloat model.windowSize.height / 2 |> px |> height
             , model.windowSize.width |> toFloat |> px |> width
             , color Css.Colors.lime
             , backgroundColor Css.Colors.black]]
        [ img [src "/logo.png"] []
        , text "Venue"
        ]
