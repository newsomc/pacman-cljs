(defproject pacman "0.1.0-SNAPSHOT"
  :description "An almost functional purely functional Pac-Man!"
  :url "http://pacmancljs.com"
  :dependencies [[org.clojure/clojure "1.4.0"]
                  [compojure "1.1.5"]
                  [hiccup "1.0.2"]]
  :plugins [[lein-ring "0.8.2"]
            [lein-cljsbuild "0.3.0"]]
  ;; enable if you want macros!
  ;; :source-paths ["src-cljs"]
  :ring {:handler pacman.routes/app}
  :profiles {:dev {:dependencies [[ring-mock "0.1.3"]]}}
  :cljsbuild {:builds
               [{:source-paths ["src-cljs/pacman"],
                  :builds nil,
                  :compiler
                  {:pretty-print true,
                    :output-to "resources/public/js/pacman.js",
                    :optimizations :advanced}}],
               :repl-listen-port 9000}
  :main pacman.routes)
