(ns pacman.helpers
  (:require [pacman.state :as state]))

;;------------------------------------------------------------------------------------------ 
;; Logging

(defn console-log [var]
  (.log js/console (pr-str var)))

;;------------------------------------------------------------------------------------------ 
;; Helpers

(defn get-element-by-id [id]
  (.getElementById js/document id))

(defn update-game
  "Updates the current game state. If we are swaping the turns vector we use update-in then
   conj the value onto our vector. If we are swaping, we assoc. 
   More about swap! here: http://clojuredocs.org/clojure_core/clojure.core/swap!"
  [key value]
  (if (= key :ctx) (swap! state/game-state update-in [:ctx] conj value)
      (swap! state/game-state assoc-in [key] value)))

(defn get-tick []
  (:tick @state/game-state))

(defn point-to-coord [x]
  (Math/round (/ x 10)))
