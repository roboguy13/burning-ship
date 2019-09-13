# Initial step

If you have not already fetched the submodule used in this project (my fork of
`elm-canvas`), run:

    git submodule update --init --recursive

# GLSL version

To make an optimized build of the GLSL version of the project, run:

    ./optimize_GL.sh src/BurningShip_GL.elm

Then open `docs/index.html` in a browser to run it.

# Non-GLSL version (*much* slower)

To make an optimized build of this project, run:

    ./optimize.sh src/BurningShip.elm

Then, open `index_non_GL.html` in a browser to run the project. Note that it will take
a little while to render.

WASD keys pan around and 'z' zooms in and 'x' zooms out (NOTE: The project is so
slow at the moment that these controls, while they seem to work, are not very
useful).

