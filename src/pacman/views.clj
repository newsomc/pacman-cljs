(ns pacman.views
  (:use [hiccup core page]))

(defn index-page []
  (html5
    [:head
     [:title "Pacman!"]
     (include-css "/css/style.css")]
    [:body
     [:h1 "Pacman!"]
     [:div {:id "pacman"}]
     ]
    (include-js "/js/jquery-1.9.1.min.js" "/js/modernizr.custom.95208.js"  "/js/cljs.js")))



