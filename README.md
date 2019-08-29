If you have not already fetched the submodule used in this project (my fork of
`elm-canvas`), run:

    git submodule update --init --recursive

To make an optimized build of this project, run:

    ./optimize.sh src/BurningShip.elm

Then, open `index.html` in a browser to run the project. Note that it will take
a little while to render.

WASD keys pan around and 'z' zooms in and 'x' zooms out (NOTE: The project is so
slow at the moment that these controls, while they seem to work, are not very
useful).

