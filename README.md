# Pac-Man CLJS

HTML5 + CLJS Pac-Man

[Somewhat Running Version](http://pacman.edgemon.org)

## Prerequisites

* Leiningen 2
* [lein-cljsbuild "0.3.0"]
* [ring-mock "0.1.3"]


## Running

Once you have the right prerequisites, run <code>lein deps</code>. 

To compile, fire up cljsbuild

	lein cljsbuild once

or if you want to build continuously 

	line cljsbuild auto 

To start a Clojure web server for the application, run

    lein ring server <desired port>
   
or point a server at the directory

	pacman/resources/public/

## Todo
1. Use requestAnimationFrame
2. Add sound
3. Fix catastrophic tunnel bug
4. Make ghost strategy more random and more efficient
5. Fix minor movement and drawing problems
6. Make work in advanced mode
