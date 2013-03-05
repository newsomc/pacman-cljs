(ns pacman.routes
  (:use [compojure.core]
        [pacman.views] 
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [] (index-page))
  (route/resources "/")
  (route/not-found "Page Not Found"))

(def app
  (handler/site app-routes))
