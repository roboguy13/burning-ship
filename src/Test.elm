import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color -- elm install avh4/elm-color
import Html exposing (Html)
import Html.Attributes exposing (style)

view : Html msg
view =
    let
        width = 500
        height = 300
    in
        Canvas.toHtml (width, height)
            []
            [ shapes [ fill Color.white ] [ rect (0, 0) width height ]
            , renderSquare
            ]

renderSquare =
  shapes [ fill Color.green ]
      [ rect (0, 0) 100 50 ]

main = view
