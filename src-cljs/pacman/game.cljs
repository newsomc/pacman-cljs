(ns pacman.core
  (:require [pacman.constants :as const]
            [pacman.gamemap :as gamemap]
            [pacman.helpers :as helper]
            [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")

;; =============================================================================
;; Definitions

(def canvas const/canvas)
(def ctx const/ctx)

(defn make-ghost [color] 
  {:get-tick 0, 
   :eatable nil, 
   :color nil, 
   :eaten nil, 
   :specs color, 
   :position nil, 
   :due nil, 
   :map nil, 
   :npos nil,
   :old-pos nil,
   :direction nil})

(def ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"])

(def key-map {(:ARROW_LEFT  const/KEYS) (:left const/game-const)
              (:ARROW_UP    const/KEYS) (:up const/game-const)
              (:ARROW_RIGHT const/KEYS) (:right const/game-const)
              (:ARROW_DOWN  const/KEYS) (:down const/game-const)})

(def game-state
  {:phase (:waiting const/game-const)
   :user {:position {:x 90 :y 120}
          :direction (:left const/game-const)
          :eaten 0
          :due (:left const/game-const)
          :lives 0
          :score 5
          :npos {:x 90 :y 120}
          :next-whole nil
          :block nil
          :old-pos nil}
   :audio []
   :ghosts (mapv make-ghost ghost-specs)
   :ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"]
   :eaten-count 0
   :level 0
   :tick 0
   :ghost-pos []
   :state-changed true
   :timer-start nil
   :last-time 0
   :ctx nil
   :timer nil
   :map nil
   :stored nil
   :n-score 0 })

;; =============================================================================
;; Draw Functions

(defn dialog [state text]
  (set! (. ctx  -fillStyle) "#FFFF00")
  (set! (. ctx -font) "14px BDCartoonShoutRegular")
  (let [dialog-width (.-width (.measureText ctx text))
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        x (/ (- (* map-width (:block-size gamemap/map-state)) dialog-width) 2)]
    (.fillText ctx text x (+ (* map-height 10) 8)))
  state)

(defn draw-score 
  [state text position]
  (set! (. ctx  -fillStyle) "#FFFFFF")
  (set! (. ctx -font) "12px BDCartoonShoutRegular")
  (.fillText ctx text (* 10 (:block-size gamemap/map-state)) (* 10 (:block-size gamemap/map-state)))
  state)

(defn draw-footer [state ctx]
  (let [block-size (:block-size gamemap/map-state)
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        top-left (* map-height block-size)
        text-base (+ top-left 17)]

    (set! (. ctx -fillStyle) "#000000")
    (.fillRect ctx 0 top-left (* map-width block-size) 30)
    (set! (. ctx -fillStyle) "#FFFF00")

    (doseq [i (range (user/get-lives))]
      (set! (. ctx -fillStyle) "#FFFF00")
      (.beginPath ctx)
      (.moveTo ctx (+ 150 (* 25 i) (/ block-size 2)) 
                   (+ (+ top-left 1) (/ block-size 2)))

      (.arc ctx (+ (+ 150 (* 25 i)) (/ block-size 2)) 
                (+ (+ top-left 1) (/ block-size 2)) 
                (/ block-size 2)
                (* (.-PI js/Math) 0.25)
                (* (.-PI js/Math) 1.75)
                false)
      (.fill ctx))

    ;; TODO: think about how to implement sound-disabled ...
    (set! (. ctx -fillStyle) (if (sound-disabled?) "#00FF00" "#FF0000"))
    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (user/the-score)) 30 text-base)
    (.fillText ctx (str "Level: " (:level state)) 260 text-base)
    state))

(defn calc-angle [dir pos]
  (cond
    (and (= dir (:right const/game-const)) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
    (and (= dir (:down  const/game-const)) (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
    (and (= dir (:up const/game-const)) (< (mod (:y pos) 10) 5))    {:start 1.25 :end 1.75 :direction true}     
    (and (= dir (:left const/game-const)) (< (mod (:x pos) 10) 5))  {:start 0.75 :end 1.25 :direction true}
    :else {:start 0 :end 2 :direction false}))

(defn redraw-block [state pos]
  (gamemap/draw-block (Math/floor (/ (:y pos) 10)) (Math/floor (/ (:x pos) 10)) (:block-size gamemap/map-state) ctx)
  (gamemap/draw-block (Math/ceil (/ (:y pos) 10)) (Math/ceil (/ (:x pos) 10)) (:block-size gamemap/map-state) ctx)
  state)

(defn draw-pacman [{user :user :as state}]
  (let [s (:block-size gamemap/map-state)
        position (:position user)
        angle (calc-angle (:direction user) position)]
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
    (.fill ctx)
    state))

(defn draw-dead 
  [state amount]
  (let [size (:block-size gamemap/map-state)
        half (/ size 2)
        position (:position (:user state))]
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
        (.fill ctx)))
    state))

(defn main-draw [state]
  (-> state
    (gamemap/draw)
    (gamemap/draw-pills)
    (draw-footer)
    (redraw-block (-> state :user :old-pos))
    (draw-pacman)))

;; =============================================================================
;; Event Handlers

(def keydown-state (atom nil))

(defn handle-keydown [e]
  (reset! keydown-state (.-keyCode e)))

;; =============================================================================
;; Update Functions & Helpers

(defn start-level [state]
  (-> state
    (assoc :timer-start (:tick state))
    (assoc :phase :countdown)))

(defn start-new-game [state]
  (-> state
    (assoc :phase :const)
    (assoc :level 1)
    (start-level)))

(defn keydown [state]
  (let [kc @keydown-state]
    (condp = kc 
      (:N const/KEYS) (start-new-game state)
      (:S const/KEYS) (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false)
      (:P const/KEYS) (-> state
                        (draw) 
                        (assoc :phase :stored))
      (:P const/KEYS) (-> state
                        (assoc :stored (:phase state))
                        (assoc :phase :pause)
                        (draw)
                        (dialog "Paused"))
      :else (if (and kc (not= (:phase state) :pause))
              (do
                (.preventDefault e)
                (.stopPropagation e)
                (assoc-in state [:user :due] (get key-map kc)))
              state))))

(defn lose-life [state])

(defn collided? [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn direction-allowable? 
  [due dir pos npos]
  (and (or (is-on-same-plane? due dir) 
           (on-grid-square? pos)) 
    (gamemap/is-floor-space? (next-pos npos due))))

(defn get-new-npos [due dir pos npos]
  (cond 
    (and (= (:y npos) 100) (>= (:x npos) 190) (= dir (:right const/game-const))) {:y 100 :x -10}
    (and (= (:y npos) 100) (<= (:x npos) -12) (= dir (:left const/game-const)))  {:y 100 :x 190}
    (= dir (:none const/game-const)) pos
    :else (if (not= due dir) (get-new-coord due pos) (get-new-coord dir pos))))

(defn facing-wall? [pos npos dir]
  (and (on-grid-square? pos) (gamemap/is-wall-space? (next-pos npos dir))))

(defn get-new-direction [due dir pos npos]
  (cond 
    (facing-wall? pos npos dir) (:none const/game-const) 
    (and (not= due dir) (direction-allowable? due dir pos npos)) due
    :else due))

(defn refresh-user-data [user] 
  (let [{due :due dir :direction
         pos :position npos :npos} user]
    {:npos     (get-new-npos due dir pos npos)
     :position  npos
     :old-pos   pos
     :direction (get-new-direction due dir pos npos)
     :due       dir}))

(defn pacman-move [user]
  (merge user (refresh-user-data user)))

(defn start-game [state]
  (dialog "Press N to start a New game")
  (-> state
    (assoc :state-changed false)))

(defn game-playing [state]
  (assoc state :phase :playing))

(defn game-dying [state]
  (if (> (- (:tick state) (:timer-start state)) (/ (const/FPS) 3))     
    (lose-life state)
    (-> state
      (update-in [:user :position] redraw-block )
      (draw-dead (/ (:tick state) (* const/FPS 2))))))

(defn game-countdown [state]
  (let [diff (+ 1 (.floor js/Math (/ (- (:timer-start state) (:tick state)) const/FPS)))]
    (if (zero? diff)
      (-> state
        (assoc :phase :playing))
      (if-not (= diff (:last-time state))
        (-> state
          (assoc-in :last-time diff)
          (dialog (str "Starting in: " diff)))))))

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

(defn eaten-pill [state]
  (-> state
    (assoc :timer-start (:tick state))
    (assoc :eaten-count 0)))

;; =============================================================================
;; Game Loop

(defn driver [state]
  (let [phase (:phase state)
        state (-> (if-not (= phase :pause) 
                    (update-in state [:tick] (fnil inc 0))
                    state)
                (keydown)
                (update-in [:user] pacman-move))]
    (main-draw
      (cond 
        (= phase :playing) state
        (and (= phase :waiting) (:state-changed state)) (start-game state) 
        (and (= phase :eaten-pause) 
          (> (- (:tick state) (:timer-start state)) (* const/FPS 2))) (game-playing state)
        (= phase :dying) (game-dying state)
        (= phase :countdown) (game-countdown state)))))

(.forEach (js/$ "div") (fn [el] ))

(defn loaded []
  (let [init-state game-state
        interval (/ 1000 const/FPS)]
    (dialog "Press N to Start")
    (.addEventListener js/document "keydown" handle-keydown true)
    (.setTimeout js/window
      (fn game-loop [s]
        (let [state (or s init-state)
              new-state (driver state)]
          (.setTimeout js/window
            #(game-loop new-state)
            interval)))
      interval)))

(defn init [wrapper root]
  (let [block-size (/ (.-offsetWidth wrapper) 19)]
    (.setAttribute canvas "width" (str (* block-size 19) "px"))
    (.setAttribute canvas "height" (str (+ (* block-size 22) 30) "px"))
    (.appendChild wrapper canvas)
    (dialog "Loading...")
    (loaded)))

;; Init!
(def elem (helper/get-element-by-id "pacman"))
(.setTimeout js/window (fn [x] (init elem "./")) 0)
