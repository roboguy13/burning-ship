import Browser
import Html exposing (Html, button, div, text, node)
import WebGL exposing (Shader, Mesh)
import Math.Vector2 exposing (Vec2, getX, getY, setX, setY, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Html.Attributes as Attributes exposing (style)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)

type Zoom = In | Out

type Command =
  Up | Down | Left | Right | Zoom Zoom | ToggleFractal

type alias Size =
  { width  : Int
  , height : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

type alias Shift = Vec2


noShift : Shift
noShift = vec2 0 0

shiftPixels : Int
shiftPixels = 4

shiftXBy : Size -> Float -> Int -> Shift -> Shift
shiftXBy {width} zoom x shift = setX (getX shift + (toFloat (x * shiftPixels) * zoom / toFloat width)) shift

shiftYBy : Size -> Float -> Int -> Shift -> Shift
shiftYBy {height} zoom y shift = setY (getY shift + (toFloat (y * shiftPixels) * zoom / toFloat height)) shift

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
  , one : Float
  , whichFractal : Float -- 0 means burning ship, 1 means Mandelbrot
  }

selectBurningShip : Float
selectBurningShip = 0

selectMandelbrot : Float
selectMandelbrot = 1

-- | Assumes that input is correctly either `selectBurningShip` or
-- `selectMandelbrot`
toggleFractal : Float -> Float
toggleFractal x =
  if x == selectBurningShip
  then selectMandelbrot
  else selectBurningShip

type alias Msg = Maybe Command

defaultViewportSize : Size
defaultViewportSize =
  let theWidth = 1280
  in
  {width = theWidth, height = 1024}

main : Program (List Int) Model Msg
main =
    Browser.element
    { init = \dims -> ((case dims of [width, height] -> {viewportSize={width=width, height=height}, drawSize={width=width, height=adjustedHeight width}, currShift=noShift, zoom=1, redraw=True, one=1, whichFractal=selectBurningShip}
                                     _ -> {viewportSize=defaultViewportSize, drawSize={width=defaultViewportSize.width, height=adjustedHeight defaultViewportSize.width}, currShift=noShift, zoom=1, redraw=True, one=1, whichFractal=selectBurningShip}), Cmd.none)
    , update = update
    , subscriptions = \_ -> onKeyDown directionKeyDecoder
    , view = view
    }

update maybeDir ({viewportSize, currShift, zoom, whichFractal} as model) =
  let shiftAmount = 30
      zoomMult = 0.9
  in
  case maybeDir of
    Nothing -> ({model | redraw = False}, Cmd.none)
    Just Up -> ({model | redraw = True, currShift = shiftYBy viewportSize zoom (-shiftAmount) currShift}, Cmd.none)
    Just Down -> ({model | redraw = True, currShift = shiftYBy viewportSize zoom shiftAmount currShift}, Cmd.none)
    Just Left -> ({model | redraw = True, currShift = shiftXBy viewportSize zoom (-shiftAmount) currShift}, Cmd.none)
    Just Right -> ({model | redraw = True, currShift = shiftXBy viewportSize zoom shiftAmount currShift}, Cmd.none)
    Just (Zoom In) -> ({model | redraw = True, zoom = zoom * zoomMult}, Cmd.none)
    Just (Zoom Out) -> ({model | redraw = True, zoom = zoom / zoomMult}, Cmd.none)
    Just ToggleFractal -> ({model | redraw = True, whichFractal = toggleFractal whichFractal}, Cmd.none)

view : Model -> Html msg
view ({viewportSize, drawSize, currShift, zoom, redraw} as model) =
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
          model]]

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

directionKeyDecoder : Decode.Decoder (Maybe Command)
directionKeyDecoder =
  Decode.map toCommand (Decode.field "key" Decode.string)

toCommand : String -> Maybe Command
toCommand str =
  case str of
    "w" -> Just Up
    "s" -> Just Down
    "a" -> Just Left
    "d" -> Just Right
    "z" -> Just (Zoom In)
    "x" -> Just (Zoom Out)
    "t" -> Just ToggleFractal
    _   -> Nothing

-- type Uniforms = Uniforms

vertexShader : Shader { position : Vec3 } Model { vFragCoord : Vec2 }
vertexShader =
  [glsl|
    varying vec2 vFragCoord;
    attribute vec3 position;

    void main() {
      gl_Position = vec4(position, 1.0);
      vFragCoord = position.xy;
    }
  |]

fragmentShader : Shader {} Model { vFragCoord : Vec2 }
fragmentShader =
  [glsl|
    precision highp float;

    varying vec2 vFragCoord;

    uniform float whichFractal;
    uniform float zoom;
    uniform vec2 currShift;
    uniform float one;

    //const int MAX_ITERS = 511;
    const int MAX_ITERS = 255;
    const float MAX_ITERS_f = float(MAX_ITERS);

    const float X_MIN = -2.5;
    const float X_MAX = 1.0;

    const float Y_MIN = -1.5;
    const float Y_MAX = 1.0;

    const float ASPECT_RATIO = (X_MAX - X_MIN) / (Y_MAX - Y_MIN);

    const float X_INCR = X_MAX - X_MIN;
    const float Y_INCR = Y_MAX - Y_MIN;


    /***** This block of code is from here (this emulates double-precision floating point arithmetic): https://github.com/10110111/QSMandel/blob/master/EmuMandel.fsh *****/
    // Emulation based on Fortran-90 double-single package. See http://crd-legacy.lbl.gov/~dhbailey/mpdist/
    // Add: res = ds_add(a, b) => res = a + b
    vec2 ds_add (vec2 dsa, vec2 dsb)
    {
    vec2 dsc;
    float t1, t2, e;

     t1 = dsa.x + dsb.x;
     e = (t1*one) - dsa.x;
     t2 = ((dsb.x - e) + (dsa.x - (t1 - e))) + dsa.y + dsb.y;

     dsc.x = t1 + t2;
     dsc.y = t2 - ((dsc.x*one) - t1);
     return dsc;
    }

    // Subtract: res = ds_sub(a, b) => res = a - b
    vec2 ds_sub (vec2 dsa, vec2 dsb)
    {
    vec2 dsc;
    float e, t1, t2;

     t1 = dsa.x - dsb.x;
     e = (t1*one) - dsa.x;
     t2 = ((-dsb.x - e) + (dsa.x - (t1 - e))) + dsa.y - dsb.y;

     dsc.x = t1 + t2;
     dsc.y = t2 - ((dsc.x*one) - t1);
     return dsc;
    }

    // Compare: res = -1 if a < b
    //              = 0 if a == b
    //              = 1 if a > b
    float ds_compare(vec2 dsa, vec2 dsb)
    {
     if (dsa.x < dsb.x) return -1.;
     else if (dsa.x == dsb.x) 
            {
            if (dsa.y < dsb.y) return -1.;
            else if (dsa.y == dsb.y) return 0.;
            else return 1.;
            }
     else return 1.;
    }

    // Multiply: res = ds_mul(a, b) => res = a * b
    vec2 ds_mul (vec2 dsa, vec2 dsb)
    {
    vec2 dsc;
    float c11, c21, c2, e, t1, t2;
    float a1, a2, b1, b2, cona, conb, split = 8193.;

     cona = dsa.x * split;
     conb = dsb.x * split;
     a1 = cona - ((cona*one) - dsa.x);
     b1 = conb - ((conb*one) - dsb.x);
     a2 = dsa.x - a1;
     b2 = dsb.x - b1;

     c11 = dsa.x * dsb.x;
     c21 = a2 * b2 + (a2 * b1 + (a1 * b2 + (a1 * b1 - c11)));

     c2 = dsa.x * dsb.y + dsa.y * dsb.x;

     t1 = c11 + c2;
     e = t1 - (c11*one);
     t2 = dsa.y * dsb.y + ((c2 - e) + (c11 - (t1 - e))) + c21;
     
     dsc.x = t1 + t2;
     dsc.y = t2 - ((dsc.x*one) - t1);
     
     return dsc;
    }

    // create double-single number from float
    vec2 ds_set(float a)
    {
     vec2 z;
     z.x = a;
     z.y = 0.0;
     return z;
    }
    /***** End of block *****/

    vec2 ds_abs(vec2 ds) {
      return vec2(abs(ds.x), abs(ds.y));
    }

    struct complex {
      vec2 re, im;
    };

    complex complexMult(complex v, complex w) {
      complex z;

      z.re = ds_sub(ds_mul(v.re, w.re), ds_mul(v.im, w.im));
      z.im = ds_add(ds_mul(v.re, w.im), ds_mul(v.im, w.re));
      return z;
    }

    complex complexAdd(complex v, complex w) {
      complex result;
      result.im = ds_add(v.im, w.im);
      result.re = ds_add(v.re, w.re);
      return result;
    }

    complex complexSquare(complex a) {
      return complexMult(a, a);
    }

    vec2 complexDot(complex v, complex w) {
      return ds_add(ds_mul(v.im, w.im), ds_mul(v.re, w.re));
    }

    complex nextZ(complex c, complex z) {
      complex absed;
      // absed.re = ds_dot(whichFractal, vec2(ds_abs(z.re), z.re));
      // absed.im = ds_dot(whichFractal, vec2(ds_abs(z.im), z.im));

      // absed.re = ds_abs(z.re);
      // absed.im = ds_abs(z.im);

      vec2 reAbs = ds_abs(z.re);
      vec2 imAbs = ds_abs(z.im);

      absed.re.x = mix(reAbs.x, z.re.x, whichFractal);
      absed.re.y = mix(reAbs.y, z.re.y, whichFractal);

      absed.im.x = mix(imAbs.x, z.im.x, whichFractal);
      absed.im.y = mix(imAbs.y, z.im.y, whichFractal);

      return complexAdd(complexSquare(absed), c);
    }

    int escapesAt(complex c) {
      complex z;
      z.im = ds_set(0.0);
      z.re = ds_set(0.0);

      for (int iters = 0; iters < MAX_ITERS; ++iters) {
        // if dot(z, z) > 4 ...
        if (ds_compare(complexDot(z,z), ds_set(4.0)) > 0.0) {
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


    vec4 coordColor(vec2 coord) {
      complex c;
      c.im = ds_set(coord.y);
      c.re = ds_set(coord.x);
      int iters = escapesAt(c);

      if (iters < 0) {
        return vec4(0, 0, 0, 1);
      } else {
        float adjIters = float(iters + 180);

        //float adjIters = float(iters + 350);

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

    vec2 zoomCoord(vec2 v) {
      return vec2(v.x * zoom, v.y * zoom);
    }

    vec2 panCoord(vec2 v) {
      return vec2(v.x + currShift.x, v.y + currShift.y);
    }

    void main() {
      gl_FragColor = coordColor(panCoord(zoomCoord(rescaleCoord(vFragCoord))));
    }
  |]

