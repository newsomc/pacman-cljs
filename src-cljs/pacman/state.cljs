(ns pacman.state
  (:require [pacman.constants :as const]))

(defn make-ghost [color] {:get-tick 0, 
                          :eatable nil, 
                          :color nil, 
                          :eaten nil, 
                          :specs color, 
                          :position nil, 
                          :due nil, 
                          :map nil, 
                          :direction nil})

(def ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"])


(def game-state (atom {:state (:waiting const/game-const)
                       :audio []
                       :ghosts (mapv make-ghost ghost-specs)
                       :ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"]
                       :eaten-count 0
                       :level 0
                       :tick 0
                       :ghost-pos []
                       :user-pos []
                       :state-changed true
                       :timer-start nil
                       :last-time 0
                       :ctx nil
                       :timer nil
                       :map nil
                       :user nil
                       :stored nil
                       :n-score 0
                       :block-size nil}))
