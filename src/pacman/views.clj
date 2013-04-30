(ns pacman.views
  (:use [hiccup core page]))

(defn index-page []
  (html5
    [:head
     [:title "Pacman!"]
     (include-css "/css/style.css")]
    [:body
     [:h1 "Cljs Pacman!"]
      [:div {:id "pacman"}
        [:canvas {:id "canvas"}]]]
    (include-js "/js/jquery-1.9.1.min.js" "/js/pacman.js")))



