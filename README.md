# pacman

HTML 5 ClJs Pac-Man

## Prerequisites

You will need [Leiningen][1] 1.7.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

    lein ring server

## Todo:
1. Use requestAnimationFrame, built into browser. This means I wont need to 

2. Instead of trying to update the game-state atom at the right time to get the draw to draw things correctly, set an is-animating field in the atom that says what animation is happening, how much time it's suppossed to take.

3. In ghost/draw, use the amount of time that's passed since the last time it was called and use that value to calculate how far into the animation we are and draw the state at that time.

** This will allow us to do things in terms of motion per second instead of motion per frame and it'll be independant of how many fps currently set...


## License?

Nope...
