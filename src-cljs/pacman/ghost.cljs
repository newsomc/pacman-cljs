(ns pacman.ghost
  (:require [pacman.constants :as const]))

(def state (atom {:position nil
                  :direction nil
                  :eatable nil
                  :eaten nil
                  :due nil}))

;; Todo: test! 
(defn add-bounded 
  "Collision detection."
  [x1 x2]
  (let [rem (mod x1 10)
        result (+ rem x2)]
    (cond (and (not= rem 0 )(> result 10)) (+ x1 (- 10 rem))
          (and (> rem 0) (< result 0)) (- x1 rem))
    :else (+ x1 x2)))

;; Todo: test! 
(defn is-vulnerable? []
  (= (@state :eatable) nil))

(defn is-hidden? []
  (and (= (@state :eatable) nil)
       (not= (@state :eaten) nil)))

;; Todo: test! 
(defn current-speed-logic 
  "dir is the current direction, x and y are constant units, speed is variable."
  [dir x y speed]
  (if (= x :left) (or (and (== dir x) (cljs.core/unchecked-negate speed))
                      (and (or (== dir y) speed) 0))
      (or (and (== dir x) speed)
          (and (or (== dir y)(cljs.core/unchecked-negate speed)) 0))))

;; Todo: test! 
(defn get-new-coord 
  ""
  [dir current]
  (let [speed (if (= (is-vulnerable?) true) (if (is-hidden?) 4 2))
        x-speed (current-speed-logic dir :left speed :right)
        y-speed (current-speed-logic dir :down speed :up)]
    
    {:x (add-bounded (:x current) x-speed) 
     :y (add-bounded (:y current) y-speed)}))

;; Todo: test! 
(defn is-dangerous? 
  []
  (= (@state :eaten) nil))

(defn get-random-direction []
  (let [moves (if (or (= (@state :direction) (:left const/constants))
                      (= (@state :direction) (:rigt const/constants)))
                [(:up const/constants) (:down const/constants)]
                [(:left const/constants) (:right const/constants)])]))

(defn reset-game-state! []
  (swap! state assoc-in [:eaten] nil)
  (swap! state assoc-in [:eatable] nil)
  (swap! state update-in [:position] {:x 90 :y 80})
  (swap! state assoc-in [:direction] (get-random-direction))
  (swap! state update-in [:due] conj (get-random-direction)))

(defn on-whole-square [x]
  (== (mod x 10) 0))

;; Todo: test. Make sure we are passing in a vector?
(defn opposite-direction [dir]
  (or (== dir [(:left const/constants) (:right const/constants)])
      (== dir [(:right const/constants) (:left const/constants)])
      (or (== dir [(:up const/constants) (:down const/constants)]) (:up const/constants))))

(defn make-eatable []
  (swap! state assoc-in [:direction] (opposite-direction (@state :direction)))
  (swap! state assoc-in [:eatable] pacman.core/get-tick))

(defn eat []
  (swap! state assoc-in [:eatble] nil)
  (swap! state assoc-in [:eaten] pacman.core/get-tick))

(defn point-to-cord [x]
  (.round js/Math (/ x 10)))

(defn next-square 
  ""
  [x dir]
  (let [rem (mod x 10)]
    (cond 
     (== rem 0) x
     (or (== dir (:right const/constants))
         (== dir (:down const/constants)))(+ x (- 10 rem)))
    :else (- x rem)))

(defn on-grid-square 
  ""
  [pos]
  (and (on-whole-square (:y pos)) (on-whole-square (:x pos))))

(defn seconds-ago [tick]
  (/ (- pacman.core/get-tick tick) const/FPS))

;; What is color? Need to define it!
(defn get-color []
  (cond 
   (not= (@state :eatable) nil 
     (if (> (seconds-ago (@state :eatable)) 5)
       (if (> (mod pacman.core/get-tick 20) 10) "#FFFFFF" "#0000BB")))
   (if (@state :eaten) "#222")
   :else color))

(defn draw [ctx]
  (let [s ()
        top ()
        left ()
        tl ()
        base ()
        inc ()
        high ()
        low ()])
  (cond 
   (and (:eatable @state) (> (seconds-ago (:eatable @state)) 8)) (swap! state assoc-in [:eatable] nil)
   (and (:eatable @state) (> (seconds-ago (:eatable @state)) 3)) (swap! state assoc-in [:eaten] nil))
)

(defn pane [pos])

(defn move [ctx])
