(ns pacman.helpers)

;;------------------------------------------------------------------------------------------ 
;; Logging

(defn console-log [var]
  (.log js/console (pr-str var)))

;;------------------------------------------------------------------------------------------ 
;; Helpers

(defn get-element-by-id [id]
  (.getElementById js/document id))

(defn set-interval
  "Invoke the given function after and every delay milliseconds."
  [delay f]
  (js/setInterval f delay))

(defn clear-interval
  "Cancel the periodic invokation specified by the given interval id."
  [interval-id]
  (js/clearInterval interval-id))

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

(defn set-state [n-state]
  (swap! state/game-state assoc-in [:state] n-state)
  (swap! state/game-state assoc-in [:state-changed] true))
