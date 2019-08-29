import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown)
import Color exposing (Color)
import Html exposing (Html, button, div, text, node)
import Html.Events exposing (onClick)

import List exposing (map, range)

import Complex exposing (toCartesian, real, complex)

import Canvas exposing (shapes, rect)
import Canvas.Settings exposing (fill, stroke)
import Html.Attributes exposing (style)

import Task exposing (perform)

import Json.Decode as Decode

type alias Complex = Complex.Complex
type alias Size =
  { width  : Int
  , height : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

type alias Shift =
  { xShift : Int
  , yShift : Int
  }

noShift : Shift
noShift = { xShift=0, yShift=0 }

shiftXBy : Int -> Shift -> Shift
shiftXBy x {xShift, yShift} = {xShift=clamp0 (xShift+x), yShift=yShift}

shiftYBy : Int -> Shift -> Shift
shiftYBy y {xShift, yShift} = {xShift=xShift, yShift=clamp0 (yShift+y)}

clamp0 : Int -> Int
clamp0 a =
  if a < 0
  then 0
  else a

type Zoom = In | Out

type Direction =
  Up | Down | Left | Right | Zoom Zoom

square : Complex -> Complex
square z = Complex.multiply z z

magnitude : Complex -> Float
magnitude z = (Complex.toPolar z).abs


nextZ : Complex -> Complex -> Complex
nextZ c z =
  let cart = toCartesian z
  in
  Complex.add (square (complex (abs cart.re)
                               (abs cart.im)))
              c

maxIters : Int
maxIters = 255

escapesAt : Complex -> Maybe Int
escapesAt c = escapesAtHelper c (real 0) 0

escapesAtHelper : Complex -> Complex -> Int -> Maybe Int
escapesAtHelper c z iters =
  if      iters >= maxIters then Nothing
  else if magnitude z > 2   then Just iters
  else                           escapesAtHelper c (nextZ c z) (iters+1)

coordColor : Complex -> Color
coordColor c =
  case escapesAt c of
    -- Just _  -> Color.white
    Just iters -> Color.hsl (toFloat (remainderBy maxIters (iters + 180)) / toFloat maxIters) 1 0.5
    Nothing    -> Color.black

xMin : Float
xMin = -2.5
xMax : Float
xMax = 1

yMin : Float
yMin = -1.5
yMax : Float
yMax = 1

aspectRatio : Float
aspectRatio = (xMax - xMin) / (yMax - yMin)

-- | Calculate a height to fit aspect ratio, given a width
adjustedHeight : Int -> Int
adjustedHeight width =
  round (toFloat width / aspectRatio)

pixelColor : Float -> Float -> Color
pixelColor x y =
  coordColor (complex x y)

genCoords : Int -> Int -> Float -> Float -> List (Int, Float)
genCoords max count start incr =
  if count == 0
  then []
  else (max - count, start) :: genCoords max (count-1) (start + incr) incr

generatePixelColors : Shift -> Float -> Size -> List (Point, Color)
generatePixelColors {xShift, yShift} zoom size =
  let xIncr = (xMax - xMin) / (toFloat size.width-1)
      yIncr = (yMax - yMin) / (toFloat size.height-1)

      xCoords =
        genCoords size.width size.width (xMin + toFloat xShift*xIncr) xIncr

      go currYInt currY =
        if currYInt >= size.height
        then []
        else map (\(xInt, x) -> ({x = xInt, y = currYInt}, pixelColor (x*zoom) (currY*zoom)))
                 xCoords
                    ++ go (currYInt+1) (currY+yIncr)
  in
      go 0 (yMin + toFloat yShift*yIncr)


drawPixels : Size -> List (Point, Color) -> Html msg
drawPixels ({width, height} as size) colors =
  div
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]
    [ text "Rendering..."
    , Canvas.cmdsToHtml size []
        [ Canvas.drawImageData (width, height) (round (toFloat width/2), round (toFloat height/2))
                               (map (\(_, c) -> c) colors)
        ]
    ]


defaultViewportSize : Size
defaultViewportSize =
  let theWidth = 1280
  in
  {width = theWidth, height = 1024}

type alias Model =
  { viewportSize : Size
  , drawSize : Size
  , currShift : Shift
  , zoom : Float
  , redraw : Bool
  }

type alias Msg = Maybe Direction

main : Program (List Int) Model Msg
main =
  Browser.element
    { init = \dims -> ((case dims of [width, height] -> {viewportSize={width=width, height=height}, drawSize={width=width, height=adjustedHeight width}, currShift=noShift, zoom=1, redraw=True}
                                     _ -> {viewportSize=defaultViewportSize, drawSize={width=defaultViewportSize.width, height=adjustedHeight defaultViewportSize.width}, currShift=noShift, zoom=1, redraw=True}), Cmd.none)
    , update = update
    , subscriptions = \_ -> onKeyDown directionKeyDecoder
    , view = view
    }

update maybeDir ({currShift, zoom} as model) =
  let shiftAmount = 30
      zoomIncr = 0.1
  in
  case maybeDir of
    Nothing -> ({model | redraw = False}, Cmd.none)
    Just Up -> ({model | redraw = True, currShift = shiftYBy shiftAmount currShift}, Cmd.none)
    Just Down -> ({model | redraw = True, currShift = shiftYBy (-shiftAmount) currShift}, Cmd.none)
    Just Left -> ({model | redraw = True, currShift = shiftXBy (-shiftAmount) currShift}, Cmd.none)
    Just Right -> ({model | redraw = True, currShift = shiftXBy shiftAmount currShift}, Cmd.none)
    Just (Zoom In) -> ({model | redraw = True, zoom = zoom - zoomIncr}, Cmd.none)
    Just (Zoom Out) -> ({model | redraw = True, zoom = zoom + zoomIncr}, Cmd.none)

view {viewportSize, drawSize, currShift, zoom, redraw} =
  drawPixels viewportSize (generatePixelColors currShift zoom viewportSize)



directionKeyDecoder : Decode.Decoder (Maybe Direction)
directionKeyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Maybe Direction
toDirection str =
  case str of
    "w"    -> Just Up
    "s"  -> Just Down
    "a"  -> Just Left
    "d" -> Just Right
    "z" -> Just (Zoom In)
    "x" -> Just (Zoom Out)
    -- _            -> Just Right
    _            -> Nothing

