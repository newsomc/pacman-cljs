# Pac-Man CLJS

HTML5 + CLJS Pac-Man

[Somewhat Working Version](http://pacman.edgemon.org)

## Prerequisites

* Leiningen 2
* [lein-cljsbuild "0.3.0"]
* [ring-mock "0.1.3"]


## Running

Once you have the right prerequisites, run <code>lein deps</code>. 

To compile, fire up cljsbuild

	lein cljsbuild once

or if you want to build continuously 

	lein cljsbuild auto 

To start a Clojure web server for the application, run

    lein ring server <desired port>
   
or point a server at the directory

	pacman/resources/public/
	
	
## Resources

[Understanding pacman ghost behavior](http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior)

## Todo
1. Use requestAnimationFrame
2. Add sound
3. Fix catastrophic tunnel bug
4. Make ghost strategy random and more efficient
5. Fix minor movement and drawing problems
6. Make game work in advanced mode
7. Remove browser repl code for production
8. Move readonly code to a map that is not recopied
9. Use defrecord for some data
