(ns pacman.user
  (:require [pacman.constants :as const]
            [pacman.state :as state]
            [pacman.helpers :as helper]
            [pacman.gamemap :as gamemap]))

(def user-state (atom {:position {:x 90 :y 120}
                       :direction nil
                       :eaten 0
                       :due nil
                       :lives 0
                       :score 5
                       :npos {:x 90 :y 120}
                       :next-whole nil
                       :block nil
                       :curr-pos nil
                       :key-map {}}))


(def key-map {(:ARROW_LEFT (:keys @const/KEY)) (:left const/game-const)
              (:ARROW_UP (:keys @const/KEY)) (:up const/game-const)
              (:ARROW_RIGHT (:keys @const/KEY)) (:right const/game-const)
              (:ARROW_DOWN (:keys @const/KEY)) (:down const/game-const)})

(defn add-score [n-score]
  (swap! user-state update-in [:score] (fnil inc n-score))
  (if (and (>= (:score @user-state) 10000) (< (- (:score @user-state) n-score) 10000))
    (swap! user-state update-in [:lives] (fnil inc 1))))

(defn the-score []
  (:score @user-state))

(defn lose-life []
   (swap! user-state update-in [:lives] (fnil dec 1)))

(defn get-lives []
  (:lives @user-state))

;; =====================================================
;; Reset user position
;; =====================================================

(defn reset-position []
  (swap! user-state assoc-in [:position] {:x 90 :y 120})
  (swap! user-state assoc-in [:direction] (:left const/game-const))
  (swap! user-state assoc-in [:due] (:left const/game-const)))

(defn new-level []
  (reset-position)
  (swap! user-state assoc-in [:eaten] 0))

(defn init-user []
  (helper/console-log "init user called!!")
  (swap! user-state assoc-in [:score] 0)
  (swap! user-state assoc-in [:lives] 3)
  (new-level))

(defn reset []
  (init-user)
  (reset-position))

;; =====================================================

(defn key-down [e] 
  (if-not (and (= (.-keyCode e) nil) (= (.-keyCode e) "undefined"))
    (do
      (let [key-code (.-keyCode e)]
        (swap! user-state assoc-in [:due] (get key-map key-code))
        (helper/console-log (get key-map key-code))
        (.preventDefault e)
        (.stopPropagation e)))))

(defn get-new-coord [dir pos]
  (helper/console-log (str "get new cord DIR: " dir " POS: " pos))
  (let [current-y (or (and (= dir (:left const/game-const)) -2)
                      (and (= dir (:right const/game-const)) 2) 
                      0)
        current-x (or (and (= dir (:down const/game-const)) -2)
                      (and (= dir (:up const/game-const)) 2) 
                      0)]
    {:x (+ (:x pos) current-x)
     :y (+ (:y pos) current-y)}))

(defn on-whole-square? [x]
  (= (mod x 10) 0))

(defn print-to-coord [n]
  (Math/round (/ n 10)))

(defn next-square [coord dir]
  (let [rem (mod coord 10)]
    (cond (= rem 0) coord
          (or (= dir (:right const/game-const)) (= dir (:down const/game-const))) (+ coord (- 10 rem))
          :else (- coord rem))))

(defn next-pos [pos dir]
  {:y (print-to-coord (next-square (:y pos) dir))
   :x (print-to-coord (next-square (:x pos) dir))})

(defn on-grid-square? [pos]
  (and (on-whole-square? (:y pos)) (on-whole-square? (:x pos))))

(defn is-on-same-plane? [due dir]
  (or (and (or (= due (:left const/game-const)) (= due (:right const/game-const))) 
           (or (= dir (:left const/game-const)) (= dir (:right const/game-const))))
      (and (or (= due (:up const/game-const)) (= due (:down const/game-const))) 
           (or (= dir (:up const/game-const)) (= dir (:down const/game-const))))))

(defn is-mid-square? [x]
  (let [rem (mod x 10)]
    (or (> rem 3) (< rem 7))))

(defn completed-level []
 (helper/set-state (:waiting const/game-const))
 (swap! state/game-state update-in [:level] (fnil inc 1))
 (gamemap/reset)
 (new-level)
 (pacman.core/start-level))

(defn eaten-pill []
  (audio/play "eatpill")
  (swap! state/game-state assoc-in [:timer] (:tick @state/game-state))
  (ghost/make-eatable!))

(defn move-user-1 [npos pos due dir]
  (swap! user-state assoc-in [:npos] (get-new-coord due pos))
  (if (and (or (is-on-same-plane? due dir) (on-grid-square? pos)) (gamemap/is-floor-space? (next-pos npos due)))  
    (swap! user-state assoc-in [:direction] due)
    (swap! user-state assoc-in [:npos] nil)))

(defn move! []

  (let [npos (:npos @user-state)
        position (:position @user-state)
        old-position position
        due (:due @user-state)
        direction (:direction @user-state)]

    (cond 
          (not= due direction) (move-user-1 npos position due direction)
          (= npos nil) (swap! user-state assoc-in [:npos] (get-new-coord direction position))
          (and (on-grid-square? position) (gamemap/is-wall-space? (next-pos npos direction))) (swap! user-state assoc-in [:direction] (:none const/game-const))
          (= direction (:none const/game-const)) (do (swap! user-state update-in [:curr-pos] conj {:new position :old position}))
          (and (= (:y npos) 100) (>= (:x npos) 190) (= direction (:right const/game-const))) (swap! user-state assoc-in [:npos] {:y 100 :x -10})
          (and (= (:y npos) 100) (<= (:x npos) -12) (= direction (:left const/game-const))) (swap! user-state assoc-in [:npos] {:y 100 :x 190}))
    
    (swap! user-state assoc-in [:position] (:npos @user-state))
    (swap! user-state assoc-in [:next-whole] (next-pos position direction))
    (swap! user-state assoc-in [:block] (gamemap/block (:next-whole @user-state)))

    (if (and (or (is-mid-square? (:y position)) (is-mid-square? (:x position)))
             (or (= (:block @user-state) const/BISCUIT) (= (:block @user-state) const/PILL)))
      (do
        (gamemap/set-block (:next-whole @user-state) const/EMPTY)
        (add-score (if (= (:block @user-state) const/BISCUIT) 10 50))
        (swap! user-state update-in [:eaten] (fnil inc 1))
        (if (= (:eaten @user-state) 182)
          (completed-level))
        (if (= (:block @user-state) const/PILL)
          (eaten-pill))))))

(defn calc-angle [dir pos]
  (cond (and (= dir (:right const/game-const)) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
        (and (= dir (:down  const/game-const)) (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
        (and (= dir (:up const/game-const)) (< (mod (:y pos) 10) 5)) {:start 1.25 :end 1.75 :direction true}     
        (and (= dir (:left const/game-const)) (< (mod (:x pos) 10) 5)) {:start 0.75 :end 1.25 :direction true}
        :else {:start 0 :end 2 :direction false}))

(defn draw-dead [ctx amount]
  (let [size (:block-size @gamemap/map-state)
        half (/ size 2)
        position (:position @user-state)]
    (if-not (>= amount 1)
      (do 
        (set! (. ctx  -fillStyle) "#FFFF00")
        (.beginPath ctx)
        (.moveTo ctx (+ (* (/ (:x position) 10) size) half)
                     (+ (* (/ (:y position) 10) size) half))

        (.arc ctx (+ (* (/ (:x position) 10) size) half)
                  (+ (* (/ (:y position) 10) size) half)
                  half
                  0
                  (* (.-PI js/Math) 2 amount)
                  true)
        (.fill ctx)))))

(defn draw []
  (let [ctx (:ctx @state/game-state)
        s (:block-size @gamemap/map-state)
        position (:position @user-state)
        angle (calc-angle (:direction @user-state) position)]

    (set! (. ctx  -fillStyle) "#FFFF00")

    (.beginPath ctx)

    (.moveTo ctx (+ (* (/ (:x position) 10) s) (/ s 2))
                 (+ (* (/ (:y position) 10) s) (/ s 2)))

    (.arc ctx (+ (* (/ (:x position) 10) s) (/ s 2))
              (+ (* (/ (:y position) 10) s) (/ s 2))
              (/ s 2)
              (* (.-PI js/Math) (:start angle))
              (* (.-PI js/Math) (:end angle))
              (:direction angle))

    (.fill ctx)))

;;(init-user)

