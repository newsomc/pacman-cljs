(ns pacman.user
  (:require [pacman.constants :as const]
            [pacman.state :as state]
            [pacman.helpers :as helper]
            [pacman.gamemap :as gamemap]))

;; ------------------------------------------------------------------------------------------------
;; State
;; ------------------------------------------------------------------------------------------------

(def user-state
  (atom { :position {:x 90 :y 120}
          :direction (:left const/game-const)
          :eaten 0
          :due (:left const/game-const)
          :lives 0
          :score 5
          :npos {:x 90 :y 120}
          :next-whole nil
          :block nil
          :old-pos nil}))

;; ------------------------------------------------------------------------------------------------
;; Direction key/map
;; ------------------------------------------------------------------------------------------------

;; Massive bug here! Up and down are switched for some reason. Look into this. 
(def key-map {(:ARROW_LEFT  (:keys @const/KEY)) (:left const/game-const)
              (:ARROW_UP    (:keys @const/KEY)) (:up const/game-const)
              (:ARROW_RIGHT (:keys @const/KEY)) (:right const/game-const)
              (:ARROW_DOWN  (:keys @const/KEY)) (:down const/game-const)})

;; ------------------------------------------------------------------------------------------------
;; Score helpers
;; ------------------------------------------------------------------------------------------------

(defn add-score 
  [n-score]
  ;(swap! user-state update-in [:score] (fnil inc n-score))
  (if (and (>= (:score @user-state) 10000) (< (- (:score @user-state) n-score) 10000))
    ;(swap! user-state update-in [:lives] (fnil inc 1))
    ))

(defn the-score 
  []
  (:score @user-state))

(defn lose-life 
  []
   ;(swap! user-state update-in [:lives] (fnil dec 1))
)

(defn get-lives 
  []
  (:lives @user-state))

;; ------------------------------------------------------------------------------------------------
;; Reset user position for start of each level.
;; ------------------------------------------------------------------------------------------------

(defn reset-position 
  []

;  (swap! user-state assoc-in [:position] {:x 90 :y 120})
;  (swap! user-state assoc-in [:direction] (:left const/game-const))
;  (swap! user-state assoc-in [:due] (:left const/game-const))
  )

(defn new-level 
  []
  (reset-position)
  ;(swap! user-state assoc-in [:eaten] 0)
)

(defn init-user 
  []
  ;(swap! user-state assoc-in [:score] 0)
  ;(swap! user-state assoc-in [:lives] 3)
  (new-level)
)

(defn reset 
  []
  (init-user)
  (reset-position))

;; ------------------------------------------------------------------------------------------------
;; Get Key pressed event.
;; ------------------------------------------------------------------------------------------------

(defn key-down 
  [e] 
  (if-not (and (= (.-keyCode e) nil) (= (.-keyCode e) "undefined"))
    (do
      (let [key-code (.-keyCode e)]
        (.log js/console key-code)
        (swap! user-state assoc-in [:due] (get key-map key-code))
        (.preventDefault e)
        (.stopPropagation e)))))

;; ------------------------------------------------------------------------------------------------
;; Keep pacman moving on the correct axis depending on the direction sent in by the user.
;; ------------------------------------------------------------------------------------------------

(defn get-new-coord 
  [dir pos]
  (let [x (or (and (= dir (:left const/game-const)) -2)
              (and (= dir (:right const/game-const)) 2) 
              0)
        y (or (and (= dir (:down const/game-const)) 2)
              (and (= dir (:up const/game-const)) -2) 
              0)]
    {:x (+ (:x pos) x)
     :y (+ (:y pos) y)}))

(defn on-whole-square? 
  [p]
  (= (mod p 10) 0))

(defn point-to-coord 
  [n]
  (Math/round (/ n 10)))

(defn next-square 
  [n dir]
  (let [rem (mod n 10)]
    (cond (= rem 0) n
          (or (= dir (:right const/game-const)) (= dir (:down const/game-const))) (+ n (- 10 rem))
          :else (- n rem))))

(defn next-pos 
  [pos dir]
  {:y (point-to-coord (next-square (:y pos) dir))
   :x (point-to-coord (next-square (:x pos) dir))})

(defn on-grid-square? 
  [pos]
  (and (on-whole-square? (:y pos)) (on-whole-square? (:x pos))))

(defn is-on-same-plane? 
  [due dir]
  (or (and (or (= due (:left const/game-const)) (= due (:right const/game-const))) 
           (or (= dir (:left const/game-const)) (= dir (:right const/game-const))))
      (and (or (= due (:up const/game-const)) (= due (:down const/game-const))) 
           (or (= dir (:up const/game-const)) (= dir (:down const/game-const))))))

(defn is-mid-square? 
  [x]
  (let [rem (mod x 10)]
    (or (> rem 3) (< rem 7))))

(defn completed-level 
  []
  ;(swap! state/game-state update-in [:level] (fnil inc 1))
  (gamemap/reset)
  (new-level)
  (pacman.core/start-level))

(defn eaten-pill 
  []
  (audio/play "eatpill")
  ;(swap! state/game-state assoc-in [:timer] (:tick @state/game-state))
  (ghost/make-eatable!))

;; ------------------------------------------------------------------------------------------------
;; Decide pacman's angle for draw func.
;; ------------------------------------------------------------------------------------------------

(defn calc-angle 
  [dir pos]
  (cond (and (= dir (:right const/game-const)) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
        (and (= dir (:down  const/game-const)) (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
        (and (= dir (:up const/game-const)) (< (mod (:y pos) 10) 5))    {:start 1.25 :end 1.75 :direction true}     
        (and (= dir (:left const/game-const)) (< (mod (:x pos) 10) 5))  {:start 0.75 :end 1.25 :direction true}
    :else {:start 0 :end 2 :direction false}))

(defn draw-dead 
  [ctx amount]
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

;; ------------------------------------------------------------------------------------------------
;; Move pacman - Helpers 
;; ------------------------------------------------------------------------------------------------

(defn direction-allowable? 
  [due dir pos npos]
  (and (or (is-on-same-plane? due dir) 
           (on-grid-square? pos)) 
    (gamemap/is-floor-space? (next-pos npos due))))

(defn get-due 
  []
  (:due @user-state))

(defn new-direction? 
  [dir]
  (= (get-due) dir))

(defn facing-wall? [pos npos dir]
  (and (on-grid-square? pos) (gamemap/is-wall-space? (next-pos npos dir))))

(defn get-new-direction 
  [due dir pos npos]
  ;; this simply gets pacman walking... (if-not (= due dir) due dir)
  (.log js/console (pr-str [pos npos due dir (facing-wall? pos npos dir)]))
  (.log js/console (direction-allowable? due dir pos npos))
  (cond 
    (facing-wall? pos npos dir) (:none const/game-const) 
    (and (not= due dir) (direction-allowable? due dir pos npos)) due
    :else due))

;(defn update-eaten [])                    

;; ------------------------------------------------------------------------------------------------
;; Compute new position
;; ------------------------------------------------------------------------------------------------

(defn get-new-npos [due dir pos npos]
  (cond 

    ;; Next two lines handle passing Pac-Man through pipes.
    (and (= (:y npos) 100) (>= (:x npos) 190) (= dir (:right const/game-const))) {:y 100 :x -10}
    (and (= (:y npos) 100) (<= (:x npos) -12) (= dir (:left const/game-const)))  {:y 100 :x 190}

    ;; If pacman is facing a wall, do not compute a new coordinate, instead, return same position.
    (= dir (:none const/game-const)) pos

    ;; If direction has changed, compute a new coordinate based on the new direction
    ;; Else, compute a new coordinate based on current direction.
    :else (if (not= due dir) (get-new-coord due pos) (get-new-coord dir pos))))

(defn refresh-user-data 
  [user] 
  {:npos (get-new-npos (:due user) (:direction user) (:position user) (:npos user))
   :position (:npos user)
   :old-pos  (:position user)
   :direction (get-new-direction (:due user) (:direction user) (:position user) (:npos user))
   :due (:direction user)

   ;; These lines fail currently.
   ;:next-whole (next-pos (:position user) (:direction user))
   ;:block (gamemap/block (:next-whole user))
   })


;; ------------------------------------------------------------------------------------------------
;; Move Pac-Man
;; ------------------------------------------------------------------------------------------------

(defn move [user]
  (reset! user-state (merge user (refresh-user-data user))))

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

(init-user)
