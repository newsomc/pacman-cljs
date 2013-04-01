(ns pacman.gamemap
  (:require [pacman.helpers :as helper]
            [pacman.constants :as const]
            [pacman.state :as state]))

(def map-state
  {:height nil
   :width nil
   :pill-size 0 
   :block-size (.-offsetWidth (helper/get-element-by-id "pacman"))
   :map const/game-map})

(defn map-pos 
  [x y]
  ;(helper/console-log (aget const/game-map x y))
  (aget const/game-map x y))

(defn within-bounds? 
  [x y]
  (and (>= y 0) (< y (:height map-state)) (>= x 0) (< x (:width map-state))))

(defn is-wall-space? 
  [pos]
  (and (within-bounds? (:x pos) (:y pos)) 
       (= const/WALL (map-pos (:y pos) (:x pos)))))

(defn is-floor-space? 
  [pos]
  (if (within-bounds? (:x pos) (:y pos))
    (let [piece (map-pos (:x pos) (:y pos))]
      (or (= piece const/EMPTY)
          (= piece const/BISCUIT)
          (= piece const/PILL)))))

 (defn draw-wall 
   [ctx]
   (set! (. ctx -strokeStyle) "#0000FF")
   (set! (. ctx -lineWidth) 5)
   (set! (. ctx -lineCap) "round")
   (letfn [(*block-size [n] (* n (:block-size map-state)))]
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

(defn block [pos]
  (map-pos (:y pos) (:x pos)))

(defn set-block 
  [pos type]
  (set! const/EMPTY (map-pos (:y pos) (:x pos))))

(defn draw-pills [ctx]

  (if (> (inc (:pill-size map-state)) 30) 
    (swap! map-state assoc-in [:pill-size] 0))
  (let [height (:height map-state)
        width (:width map-state)
        block-size (:block-size map-state)]
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
                      (Math/abs (- 5 (/ (:pill-size map-state) 3)))
                      0
                      (* (.-PI js/Math) 2)
                      false)
            (.fill ctx)
            (.closePath ctx)))))))

(defn do-draw-biscuit [y x layout block-size ctx]
  (set! (. ctx -fillStyle) "#000")
  (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)
  (if (= layout const/BISCUIT)
    (do
      (set! (. ctx -fillStyle) "#FFF")
      (.fillRect ctx (+ (* x block-size) (/ block-size 2.5)) 
                     (+ (* y block-size) (/ block-size 2.5))
                     (/ block-size 6)
                     (/ block-size 6)))))

(defn draw-block [y x block-size ctx] 
  (let [layout (map-pos y x)]
    (if-not (= layout const/PILL) 
      (do   
        (.beginPath ctx)
        (if (or (= layout const/EMPTY)
                (= layout const/BLOCK)
                (= layout const/BISCUIT)) 
          (do-draw-biscuit y x layout block-size ctx))
        (.closePath ctx)))))

(defn draw 
  "Main draw function for game board."
  [ctx]
  (set! (. ctx  -fillStyle) "#000")
  (let [width (:width map-state)
        height (:height map-state)
        size (:block-size map-state)]
    (.fillRect ctx 0 0 (* width size) (* height size))
    (draw-wall ctx)
    (doseq [i (range height)] 
      (doseq [j (range width)]
        (draw-block i j size ctx)))))

(defn reset []
  (swap! map-state assoc-in [:map] const/game-map)
  (swap! map-state assoc-in [:height] (alength const/game-map))
  (swap! map-state assoc-in [:width] (alength (aget const/game-map 0))))

(reset)


