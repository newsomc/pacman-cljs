(ns pacman.game
;; (:require-macros [pacman.macros :as m])
  (:require [pacman.constants :as const]
            [pacman.helpers :as helper]
            [goog.dom :as dom]
            [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")

;; =============================================================================
;; Definitions

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))
(def controls { (:ARROW_LEFT  const/KEYS) :left
                (:ARROW_UP    const/KEYS) :up
                (:ARROW_RIGHT const/KEYS) :right
                (:ARROW_DOWN  const/KEYS) :down })

;; Game State
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

(def game-state
  {:phase :waiting
   :dialog nil
   :countdown 4
   :user {:position nil
          :old-pos nil
          :direction nil
          :due :left
          :speed 2
          :eaten 0
          :lives 3
          :score 0
          :next-whole nil
          :block nil}
   :map { :height nil
          :width nil
          :pill-size 0 
          :block-size nil
          :board const/game-map}
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
   :stored nil
   :n-score 0})

;; =============================================================================
;; Draw Functions

(defn draw-dialog [{map :map dialog :dialog :as state}]
  (if dialog
    (do
      (set! (. ctx  -fillStyle) "#FFFF00")
      (set! (. ctx -font) "14px BDCartoonShoutRegular")
      (let [dialog-width (.-width (.measureText ctx dialog))
            map-width 19
            map-height 22
            x (/ (- (* map-width (:block-size map)) dialog-width) 2)]
        (.fillText ctx dialog x (+ (* map-height 10) 8))
        state)))
  state)

(defn draw-score 
  [{map :map :as state} text position]
  (set! (. ctx  -fillStyle) "#FFFFFF")
  (set! (. ctx -font) "12px BDCartoonShoutRegular")
  (.fillText ctx text (* 10 (:block-size map)) (* 10 (:block-size map)))
  state)

(declare draw-block draw-pills)

(defn draw-footer [{map :map user :user :as state}]
  (let [block-size (:block-size map)
        map-width (count (get const/game-map 0))
        map-height (count const/game-map)
        top-left (* map-height block-size)
        text-base (+ top-left 17)]
    (set! (. ctx -fillStyle) "#000000")
    (.fillRect ctx 0 top-left (* map-width block-size) 30)
    (set! (. ctx -fillStyle) "#FFFF00")
    (doseq [i (range (:lives user))]
      (set! (. ctx -fillStyle) "#FFFF00")
      (doto ctx
        (.beginPath)
        (.moveTo (+ 150 (* 25 i) (/ block-size 2)) 
                 (+ (+ top-left 1) (/ block-size 2)))
        (.arc (+ (+ 150 (* 25 i)) (/ block-size 2)) 
              (+ (+ top-left 1) (/ block-size 2)) 
              (/ block-size 2)
              (* (.-PI js/Math) 0.25)
              (* (.-PI js/Math) 1.75)
              false)
        (.fill)
        state))

    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (:score user)) 30 text-base)
    (.fillText ctx (str "Level: " (:level state)) 260 text-base)
    state))

;; =============================================================================
;; Draw Pac-Man

(defn calc-angle [dir pos]
  (cond
    (and (= dir :right) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
    (and (= dir :down)  (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
    (and (= dir :up)    (< (mod (:y pos) 10) 5)) {:start 1.25 :end 1.75 :direction true}     
    (and (= dir :left)  (< (mod (:x pos) 10) 5)) {:start 0.75 :end 1.25 :direction true}
    :else {:start 0 :end 2 :direction false}))

(defn redraw-block [{map :map :as state}]
  (let [{{pos :position} :user} state
        bs (:block-size map)
        fy (Math/floor (/ (:y pos) 10))
        fx (Math/floor (/ (:x pos) 10))
        cy (Math/ceil (/ (:y pos) 10))
        cx (Math/ceil (/ (:x pos) 10))]
    (draw-block state fy fx bs)
    (draw-block state cy cx bs)
    state))

(defn draw-pacman [{map :map user :user :as state}]
  (let [s        (:block-size map)
        position (:position user)
        angle    (calc-angle (:direction user) position)]
    (set! (. ctx  -fillStyle) "#FFFF00")
    (doto ctx
      (.beginPath)
      (.moveTo (+ (* (/ (:x position) 10) s) (/ s 2))
        (+ (* (/ (:y position) 10) s) (/ s 2)))
      (.arc (+ (* (/ (:x position) 10) s) (/ s 2))
        (+ (* (/ (:y position) 10) s) (/ s 2))
        (/ s 2)
        (* (.-PI js/Math) (:start angle))
        (* (.-PI js/Math) (:end angle))
        (:direction angle))
      (.fill))
    state))

(defn draw-dead [{map :map :as state} amount]
  (let [size (:block-size map)
        half (/ size 2)
        position (:position (:user state))]
    (if-not (>= amount 1)
      (set! (. ctx  -fillStyle) "#FFFF00")
      (doto ctx
        (.beginPath)
        (.moveTo (+ (* (/ (:x position) 10) size) half)
          (+ (* (/ (:y position) 10) size) half))

        (.arc (+ (* (/ (:x position) 10) size) half)
          (+ (* (/ (:y position) 10) size) half)
          half
          0
          (* (.-PI js/Math) 2 amount)
          true)
        (.fill)))
    state))

(declare draw-wall draw-block is-floor-space? move-pacman)

(defn draw-map [{map :map :as state}]
  (set! (. ctx  -fillStyle) "#000")
  (let [width  (:width map)
        height (:height map)
        size   (:block-size map)]
    (.fillRect ctx 0 0 (* width size) (* height size))
    (draw-wall map)
    (doseq [i (range height)] 
      (doseq [j (range width)]
        (draw-block state i j size)))
    state))

(declare set-block-eaten)

(defn main-draw [state]
  (let [new-state (-> state
                    (draw-map)
                    (draw-footer)
                    (draw-pills)
                    (draw-dialog))]
    (if (= :playing (:phase state))
      (-> new-state
        (move-pacman)
        (set-block-eaten)
        (redraw-block)
        (draw-pacman))
      state)))

;; =============================================================================
;; Event Handlers

(def keydown-state (atom nil))

(defn handle-keydown [e]
    (reset! keydown-state (.-keyCode e))
    (.preventDefault e)
    (.stopPropagation e))

;; =============================================================================
;; Update Functions & Helpers

(defn start-level [state]
  (-> state
    (assoc :timer-start (- (:tick state) 1))
    (assoc :phase :countdown)))

(defn start-new-game [state]
  (-> state
    (assoc :level 1)
    (start-level)))

(defn resume-game [state]
  (-> state 
    (assoc :dialog nil)
    (assoc :phase :playing)))

(defn pause-game [state]
    (-> state
      (assoc :dialog "Paused")
      (assoc :phase :pause)))

(defn toggle-pause [state]
  (if (= (:phase state) :pause)
    (resume-game state)
    (pause-game state)))

(defn keydown [state]
  (let [kc @keydown-state]
    (reset! keydown-state nil)
    (condp = kc
      (:N const/KEYS) (start-new-game state)
      (:S const/KEYS) (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false)
      (:P const/KEYS) (toggle-pause state)
      (if (and kc (not= (:phase state) :pause))
        (do 
          (assoc-in state [:user :due] (get controls kc)))
        state))))

(defn lose-life [state])

(defn collided? [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

;; is this still needed? - clint
(defn is-on-same-plane? [dir]
  (or (and (or (= dir :left) (= dir :right)))
      (and (or (= dir :up)   (= dir :down)))))

(defn on-whole-square? [n]
  (= (mod n 10) 0))

(defn on-grid-square? [{x :x y :y }]
  (and (on-whole-square? y) (on-whole-square? x)))

(defn point-val-to-coord [n] 
  (Math/round (/ n 10)))

(defn point-to-coord [{x :x y :y }]
  {:x (point-val-to-coord x) :y (point-val-to-coord y)})

(defn next-coord [{x :x y :y} dir]
  (case dir
    :left { :x (- x 1) :y y }
    :right { :x (+ x 1) :y y }
    :up { :x x :y (- y 1) }
    :down { :x x :y (+ y 1) }
    {:x x :y y})) 

(defn next-square [n dir]
  (let [rem (mod n 10)]
    (cond
      (= rem 0) n
      (or (= dir :right) (= dir :down)) (+ n (- 10 rem))
      :else (- n rem))))

(defn next-pos [{x :x y :y} dir]
  {:y (point-val-to-coord (next-square y dir))
   :x (point-val-to-coord (next-square x dir))})

;; Also used for moving Pac-Man. Helps determine if he is facing a wall.
;; Could be doing too many things at once...
(defn direction-allowable? [map dir pos]
  (and (or (is-on-same-plane? dir) 
           (on-grid-square? pos)) 
    (is-floor-space? map (next-coord (point-to-coord pos) dir))))

(defn map-pos [y x]
  (get (get const/game-map y) x))

(defn board-pos [map y x]
  (get (get (:board map) y) x))

(defn within-bounds? [map x y]
  (and (>= y 0) (< y (:height map)) 
       (>= x 0) (< x (:width map))))

(defn is-wall-space? [map {x :x y :y}]
  (and (within-bounds? map x y) 
       (= const/WALL (board-pos map y x))))

(defn is-floor-space? [map {x :x y :y}]
  (if (within-bounds? map x y)
    (let [piece (board-pos map y x)]
      (or (= piece const/EMPTY)
          (= piece const/BISCUIT)
          (= piece const/PILL)))))

(defn draw-wall [map]
  (set! (. ctx -strokeStyle) "#0000FF")
  (set! (. ctx -lineWidth) 5)
  (set! (. ctx -lineCap) "round")
  (letfn [(*block-size [n] (* n (:block-size map)))]
    (doseq [line const/WALLS]
      (.beginPath ctx)
      (doseq [point line]
        (cond (:move point) (let [[a b] (:move point)]
                              (.moveTo ctx (*block-size a) (*block-size b)))
          (:line point) (let [[a b] (:line point)] 
                          (.lineTo ctx (*block-size a) (*block-size b)))
          (:curve point) (let [[a b c d] (:curve point)] 
                           (.quadraticCurveTo ctx 
                             (*block-size a)
                             (*block-size b)
                             (*block-size c)
                             (*block-size d)))))       
      (.stroke ctx))))

(defn draw-pills [{map :map :as state}]
  (let [height     (:height map)
        width      (:width map)
        block-size (:block-size map)]
    (doseq [i (range height)]
      (doseq [j (range width)]
        (if (= (map-pos i j) const/PILL)
          (do
            (.beginPath ctx)
            (set! (. ctx -fillStyle) "#000")
            (.fillRect ctx (* j block-size) 
                           (* i block-size)
                           block-size 
                           block-size)
            (set! (. ctx -fillStyle) "#FFF")
            (.arc ctx (+ (* j block-size) (/ block-size 2))
                      (+ (* i block-size) (/ block-size 2))
                     (Math/abs (- 5 (/ (:pill-size map) 3)))
                      0
                      (* (.-PI js/Math) 2)
                      false)
            (.fill ctx)
            (.closePath ctx)))))
    state))

(defn draw-biscuit [y x layout {map :map :as state}]
  (let [block-size (:block-size map)]
    (set! (. ctx -fillStyle) "#000")
    (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)
    (if (= layout const/BISCUIT)
      (do
        (set! (. ctx -fillStyle) "#FFF")
        (.fillRect ctx (+ (* x block-size) (/ block-size 2.5)) 
          (+ (* y block-size) (/ block-size 2.5))
          (/ block-size 6)
          (/ block-size 6))))
    state))

(defn draw-block [{map :map :as state} y x block-size] 
  (let [layout (board-pos map y x)]
    (if-not (= layout const/PILL) 
      (do   
        (.beginPath ctx)
        (if (or (= layout const/EMPTY)
                (= layout const/BLOCK)
                (= layout const/BISCUIT)) 
          (draw-biscuit y x layout state))
        (.closePath ctx)))
    state))

(defn block 
  "Turns x and y coordinates into tile value in the game board."
  [map {x :x y :y}]
  (board-pos map y x))

(defn add-score [{user :user map :map :as state}]
  (let [score (:score user)
        {pos :position dir :direction} user
        next-whole (next-pos pos dir)
        block-pos (block map next-whole)
        nscore (if (= block-pos const/BISCUIT) 0 10)
        s (-> state
            (assoc-in [:user :score] (+ nscore (:score user))))]
    (if (and (>= score 10000) (< (- (:score user) nscore) 10000))
      (-> s
        (update-in [:user :lives] (fnil inc 0)))
      s)))

;; ============================================================================================
;; Move Pac-Man

(defn get-new-coord [dir {x :x y :y} speed]
  (case dir
    :left  {:x (- x speed) :y y}
    :right {:x (+ x speed) :y y}
    :up    {:x x :y (- y speed)}
    :down  {:x x :y (+ y speed)}
    {:x x :y y}))

(defn get-new-pos [dir pos speed]
  (cond 
    ;(and (= (:y pos) 100) (>= (:x pos) 190) (= dir :right)) {:y 100 :x -10}
    ;(and (= (:y pos) 100) (<= (:x pos) -12) (= dir :left))  {:y 100 :x 190}
    :else (get-new-coord dir pos speed)))


(defn get-new-direction [map due dir pos]
  (cond 
    (and dir (is-wall-space? map 
             (next-coord (point-to-coord pos) due)) 
             (not (is-wall-space? map (next-coord (point-to-coord pos) dir)))) dir
    (direction-allowable? map due pos) due))

(defn set-block [{x :x y :y} map type]
  (let [row (get map y)
        new-row (assoc row x type)]
    (assoc map y new-row)))

(defn set-block-eaten [{user :user map :map :as state}]
  (let [{pos :position dir :direction} user
        {board :board} map
        next-whole (next-pos pos dir)
        block-pos (block map next-whole)]
    (if (= block-pos const/BISCUIT)      
      (-> state 
        (assoc-in [:map :board] (set-block next-whole board const/EMPTY))
        (update-in [:user :eaten] (fnil inc 0))
        (add-score))      
      state)))

(defn nearest-10 [n]
  (* 10 (Math/round (/ n 10))))

(defn normalize-position [dir {x :x y :y}]
  (case dir 
    :left {:x x :y (nearest-10 y)}    
    :right {:x x :y (nearest-10 y)}    
    :up {:x (nearest-10 x) :y y}
    :down {:x (nearest-10 x) :y y}
    {:x x :y y}))

(defn refresh-user-data [{user :user map :map :as state}] 
  (let [{dir :direction pos :position 
         due :due speed :speed} user
         ndir (get-new-direction map due dir pos)
         npos (get-new-pos ndir (normalize-position dir pos) speed)]    
    (if (= (:phase state) :playing)
      { :position  npos
        :old-pos   pos
        :direction ndir}
      {})))

(defn move-pacman [state]
  (update-in state [:user]
    (fn [user]
      (merge user (refresh-user-data state)))))

;; ============================================================================================
;; Game Phases

(defn start-game [state]
  (-> state
    (assoc :state-changed false)))

(defn game-playing [state]
  (-> state
    (assoc :phase :playing)))

(defn game-dying [state]
  (if (> (- (:tick state) (:timer-start state)) (/ (const/FPS) 3))     
    (lose-life state)
    (-> state
      (redraw-block)
      (draw-dead (/ (:tick state) (* const/FPS 2))))))

(def ticks-remaining (atom 0))

(defn game-countdown [state]
  (if (zero? @ticks-remaining)
    (if (= (:countdown state) 1)
      (do
        (reset! ticks-remaining 0)
        (-> state game-playing (assoc :dialog nil) (assoc :countdown 4)))
      (do 
        (reset! ticks-remaining const/FPS)
        (-> state 
          (update-in [:countdown] dec) 
          (assoc :dialog (str "Starting in: " (dec (:countdown state)))))))
    (do
      (swap! ticks-remaining dec)
      state)))
 
(defn eaten-pill [state]
  (-> state
    (assoc :timer-start (:tick state))
    (assoc :eaten-count 0)))

;; ============================================================================================
;; Main Game Loop

(defn driver  [state]
  (let [phase (:phase state)
        state (-> (if (= phase :pause) 
                    state
                    (update-in state [:tick] (fnil inc 0)))
                (keydown)
                ;(update-in [:user] move-pacman)
)]
    (main-draw
      (cond 
        (= phase :playing) state;(game-playing state)
        (and (= phase :waiting) (:state-changed state)) (start-game state) 
        (and (= phase :eaten-pause) 
          (> (- (:tick state) (:timer-start state)) (* const/FPS 2))) (game-playing state)
        (= phase :dying) (game-dying state)
        (= phase :countdown) (game-countdown state)
        :else state))))

(defn make-state []
  (-> game-state
    (assoc :dialog "Press N to start a new game")
    (assoc-in [:user :position] {:x 90 :y 120})
    (assoc-in [:map :width] 19)
    (assoc-in [:map :height] 22)
    (assoc-in [:map :block-size] 18)))

(defn loaded []
  (let [init-state (make-state)
        interval   (/ 1000 const/FPS)]
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
    ;; a bit trickier maybe handle as special? - David
    ;;(dialog {} "Loading...")
    (loaded)))

;; Init!
(def elem (helper/get-element-by-id "pacman"))
(.setTimeout js/window (fn [x] (init elem "./")) 0)
