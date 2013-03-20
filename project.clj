(defproject pacman "0.1.0-SNAPSHOT"
  :description "Pacman for cljs!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.2"]
                 [jayq "2.1.0"]]
  :plugins [[lein-ring "0.8.2"]
            [lein-beanstalk "0.2.7"]
            [lein-cljsbuild "0.2.9"]]
  :ring {:handler pacman.routes/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}}
  :cljsbuild
  {:builds
   [{:builds nil,
     :source-path "src-cljs/pacman",
     :compiler
     {:pretty-print true,
      :output-to "resources/public/js/cljs.js",
      :optimizations :simple}
     }
    ]
   }
  :main pacman.routes)