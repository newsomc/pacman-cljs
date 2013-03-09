(ns pacman.ghost
  (:require [pacman.constants :as const]
            [pacman.helpers :as helper]
            [pacman.gamemap :as gamemap]
            [pacman.state :as state]))

(defn add-bounded 
  "Collision detection."
  [x1 x2]
  (let [rem (mod x1 10)
        result (+ rem x2)]
    (cond (and (not= rem 0 )(> result 10)) (+ x1 (- 10 rem))
          (and (> rem 0) (< result 0)) (- x1 rem))
    :else (+ x1 x2)))

(defn is-vulnerable? [ghost]
  (= (:eatable ghost) nil))

(defn is-hidden? [ghost]
  (and (= (:eatable ghost) nil)
       (not= (:eaten ghost) nil)))

(defn current-speed-logic 
  "Dir is the current direction, x and y are constant units, speed is variable."
  [dir x y speed]
  (if (= x :left) (or (and (== dir x) (cljs.core/unchecked-negate speed))
                      (and (or (== dir y) speed) 0))
      (or (and (== dir x) speed)
          (and (or (== dir y)(cljs.core/unchecked-negate speed)) 0))))

(defn get-new-coord 
  ""
  [ghost dir current]
  (let [speed (if (= (is-vulnerable? ghost) true) (if (is-hidden? ghost) 4 2))
        x-speed (current-speed-logic dir :left speed :right)
        y-speed (current-speed-logic dir :down speed :up)]

    {:x (add-bounded (:x current) x-speed) 
     :y (add-bounded (:y current) y-speed)}))

(defn is-dangerous? 
  [ghost]
  (= (:eaten ghost) nil))

(defn get-random-direction [ghost]
  (let [moves (if (or (= (:direction ghost) (:left const/game-const))
                      (= (:direction ghost) (:rigt const/game-const)))
                [(:up const/game-const) (:down const/game-const)]
                [(:left const/game-const) (:right const/game-const)])]))

(defn reset-game-state! []
  (doseq [ghost (:ghosts @state/game-state)]
    (swap! state/game-state assoc-in [:eaten] nil)
    (swap! state/game-state assoc-in [:eatable] nil)
    (swap! state/game-state assoc-in [:position] {:x 90 :y 80})
    (swap! state/game-state assoc-in [:direction] (get-random-direction ghost))
    (swap! state/game-state assoc-in [:due] (get-random-direction ghost))))

(defn on-whole-square [x]
  (== (mod x 10) 0))

(defn opposite-direction 
  "Send the ghost scurrying off in the opposite direction."
  [dir]
  (or (== dir [(:left const/game-const) (:right const/game-const)])
      (== dir [(:right const/game-const) (:left const/game-const)])
      (or (== dir [(:up const/game-const) (:down const/game-const)]) (:up const/game-const))))

(defn make-eatable [ghost]
    (swap! ghost assoc-in [:direction] (opposite-direction (:direction ghost)))
    (swap! ghost assoc-in [:eatable] helper/get-tick))

(defn eat [ghost]
  (swap! ghost assoc-in [:eatble] nil)
  (swap! ghost assoc-in [:eaten] helper/get-tick))

(defn point-to-cord [x]
  (.round js/Math (/ x 10)))

(defn next-square 
  [x dir]
  (let [rem (mod x 10)]
    (cond 
     (== rem 0) x
     (or (== dir (:right const/game-const))
         (== dir (:down const/game-const)))(+ x (- 10 rem)))
    :else (- x rem)))

(defn on-grid-square 
  ""
  [pos]
  (and (on-whole-square (:y pos)) (on-whole-square (:x pos))))

(defn seconds-ago 
  ""
  [tick]
  (/ (- helper/get-tick tick) const/FPS))

;; What is color? Need to define it!
(defn get-color [ghost]
  (cond 
   (not= (:eatable ghost) nil 
     (if (> (seconds-ago (:eatable ghost)) 5)
       (if (> (mod helper/get-tick 20) 10) "#FFFFFF" "#0000BB")))
   (if (:eaten ghost) "#222")
   :else (:color ghost)))

(defn draw [ghost ctx]
  (let [position (:position ghost)
        s (:block-size (:map ghost))
        eatable (:eatable ghost)
        top (* (/ (:y position) 10) 2)
        left (* (/ (:x position) 10) 2)
        tl (+ left s)
        base (- (+ top s) 3)
        inc (/ s 10)
        high (if (> (mod helper/get-tick 10) 5) 3 -3)
        low (if (> (mod helper/get-tick 10) 5) -3 3)
        direction (:direction ghost)
        f (/ s 12)
        off {}]
    (cond 
     (and eatable (> (seconds-ago eatable) 8)) (swap! ghost assoc-in [:eatable] nil)
     (and eatable (> (seconds-ago eatable) 3)) (swap! ghost assoc-in [:eaten] nil))
   
    (set! (. ctx -fillStyle) (get-color ghost))
    (.beginPath ctx)
    (.moveTo ctx left base)
    (.quadraticCurveTo ctx left top (+ left (/ s 2)) top)
    (.quadraticCurveTo ctx (+ left s) top (+ left s) base)
    
    (.quadraticCurveTo (- tl (* inc 1)) (+ base high) (- (* inc 2) tl) base)
    (.quadraticCurveTo (- tl (* inc 3)) (+ base low)  (- (* inc 4) tl) base)
    (.quadraticCurveTo (- tl (* inc 5)) (+ base high) (- (* inc 6) tl) base)
    (.quadraticCurveTo (- tl (* inc 7)) (+ base low)  (- (* inc 8) tl) base)
    (.quadraticCurveTo (- tl (* inc 9)) (+ base low)  (- (* inc 8) tl) base)
    
    (.closePath ctx)
    (.fill ctx)
    (.beginPath ctx)
    (set! (. ctx -fillStyle) "#FFF")
    (.arc ctx (+ left 6) (+ top 6) (/ s 6) 0 300 false)
    (.arc ctx (- (+ left s) 6) (+ top 6) (/ s 6) 0 300 false)
    (.closePath ctx)
    (.fill ctx)
    
    (conj off {:right [f 0] :left [(cljs.core/unchecked-negate f)  0] :up [0 (cljs.core/unchecked-negate f)] :down [0 f]})

    (.beginPath ctx)
    (set! (. ctx -fillStyle) "#000")
    (.arc ctx (+ left 6 (nth (direction off) 0)) (+ left 6 (nth (direction off) 1)) (/ s 15) 0 300 false)
    (.arc ctx (+ (- (+ left s) 6) (nth (direction off) 0)) (+ left 6 (nth (direction off) 1)) (/ s 15)  300 false)
    (.closePath ctx)
    (.fill ctx)))

(defn pane [ghost pos]
  (cond 
   (and (= (:y pos) 100) ( >= (:x pos) 190) (= (:direction ghost) (:right const/game-const)) {:y 100 :x -10}) 
   (and (= (:y pos) 100) (<= (:x pos) -10) (= (:direction ghost) (:left const/game-const)) {:y 100 :x 190})))

(defn move-ghost [ghost due on-grid]
  (swap! ghost assoc-in [:npos] (get-new-coord ghost due (:position ghost)))
  (if (and (on-grid) (map/is-floor-space {:y (helper/point-to-coord (next-square (:y (:npos ghost)) due)) 
                                          :x (helper/point-to-coord (next-square (:x (:npos ghost)) due))})) 
    (swap! ghost assoc-in [:direction] due)))

(defn move [ghost ctx]
  (let [direction (:direction ghost)
        old-pos (:position ghost)
        on-grid (on-grid-square (:position ghost))
        due (:due ghost)
        tmp (pane ghost (:position ghost))]
    (if (not= due direction) 
      (move-ghost ghost due on-grid)
      (swap! ghost assoc-in [:due] nil))
    (if (nil? (:npos ghost)) (swap! ghost assoc-in [:npos] (get-new-coord ghost (:direction ghost) (:position ghost))))
    (if (and (on-grid) (map/is-wall-space {:y (helper/point-to-coord (next-square (:y (:npos ghost)) direction) ) 
                                           :x (helper/point-to-coord (next-square (:x (:npos ghost)) direction))}))
      (swap! ghost assoc-in [:due] (get-random-direction ghost)))
    (swap! ghost assoc-in [:position] (:nps ghost))
    (if-not (nil? tmp) (swap! ghost assoc-in [:position] tmp))
    (swap! ghost assoc-in [:due] (get-random-direction ghost))))
