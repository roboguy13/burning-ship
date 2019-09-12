import Browser
import Html exposing (Html, button, div, text, node)
import WebGL exposing (Shader, Mesh)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Html.Attributes as Attributes exposing (style)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)

type Zoom = In | Out

type Direction =
  Up | Down | Left | Right | Zoom Zoom

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

type alias Model =
  { viewportSize : Size
  , drawSize : Size
  , currShift : Shift
  , zoom : Float
  , redraw : Bool
  }

type alias Msg = Maybe Direction

defaultViewportSize : Size
defaultViewportSize =
  let theWidth = 1280
  in
  {width = theWidth, height = 1024}

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

view : Model -> Html msg
view {viewportSize, drawSize, currShift, zoom, redraw} =
  div
    []
    [WebGL.toHtml
        [ Attributes.width viewportSize.width
        , Attributes.height viewportSize.height
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "display" "block"
        ]
        [WebGL.entity
          vertexShader
          fragmentShader
          mesh
          Uniforms]]

mesh : Mesh { position : Vec3 }
mesh =
  WebGL.triangles
      [ ( { position = vec3 -1 1 0 }
        , { position = vec3 1 1 0 }
        , { position = vec3 -1 -1 0 }
        )
      , ( { position = vec3 -1 -1 0 }
        , { position = vec3 1 1 0 }
        , { position = vec3 1 -1 0 }
        )
      ]

directionKeyDecoder : Decode.Decoder (Maybe Direction)
directionKeyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Maybe Direction
toDirection str =
  case str of
    "w" -> Just Up
    "s" -> Just Down
    "a" -> Just Left
    "d" -> Just Right
    "z" -> Just (Zoom In)
    "x" -> Just (Zoom Out)
    _   -> Nothing

type Uniforms = Uniforms

vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
  [glsl|
    varying vec2 vFragCoord;
    attribute vec3 position;

    void main() {
      gl_Position = vec4(position, 1.0);
      vFragCoord = position.xy;
    }
  |]

fragmentShader : Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
  [glsl|
    precision mediump float;
    varying vec2 vFragCoord;

    // const int MAX_ITERS = 511;
    const int MAX_ITERS = 255;
    const float MAX_ITERS_f = float(MAX_ITERS);

    const float X_MIN = -2.5;
    const float X_MAX = 1.0;

    const float Y_MIN = -1.5;
    const float Y_MAX = 1.0;

    const float ASPECT_RATIO = (X_MAX - X_MIN) / (Y_MAX - Y_MIN);

    const float X_INCR = X_MAX - X_MIN;
    const float Y_INCR = Y_MAX - Y_MIN;

    vec2 complexMult(vec2 a, vec2 b) {
      return vec2(a.x*b.x - a.y*b.y, a.x*b.y + a.y*b.x);
    }

    vec2 complexSquare(vec2 a) {
      return complexMult(a, a);
    }

    vec2 nextZ(vec2 c, vec2 z) {
      return complexSquare(vec2(abs(z.x), abs(z.y))) + c;
    }

    int escapesAt(vec2 c) {
      vec2 z = vec2(0, 0);

      for (int iters = 0; iters < MAX_ITERS; ++iters) {
        if (length(z) > 2.0) {
          return iters;
        }

        z = nextZ(c, z);
      }

      return -1;
    }

    // This function is from https://www.shadertoy.com/view/XljGzV
    vec3 hsl2rgb( in vec3 c )
    {
        vec3 rgb = clamp( abs(mod(c.x*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0, 0.0, 1.0 );

        return c.z + c.y * (rgb-0.5)*(1.0-abs(2.0*c.z-1.0));
    }


    vec4 coordColor(vec2 c) {
      int iters = escapesAt(c);

      if (iters < 0) {
        return vec4(0, 0, 0, 1);
      } else {
        float adjIters = float(iters + 180);
        return vec4(hsl2rgb(vec3(
                mod(adjIters, MAX_ITERS_f) / MAX_ITERS_f,
                1,
                0.5
                )
            ),
          1);
      }
    }

    vec2 rescaleCoord(vec2 v) {
      return vec2(v.x * X_INCR, v.y * -Y_INCR);
    }

    void main() {
      gl_FragColor = coordColor(rescaleCoord(vFragCoord));
    }
  |]

