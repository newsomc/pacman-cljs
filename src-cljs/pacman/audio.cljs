(ns pacman.audio
  (:require [pacman.helpers :as helper]))

(def audio-state (atom {:files []
                        :end-events []
                        :progress-events []
                        :playing []}))

(defn progress [event name callback]
  (and (== ))
)

(defn load [f-name path cb]
  (swap! audio-state assoc-in [:files] {:name f-name :elem (.createElement js/document "audio")})
  (swap! audio-state assoc-in [:progress-events] {f-name (fn [event] (progress event f-name cb))})
  (let [{fname :name elem :elem} (:files @audio-state)]
    (.addEventListener elem "canplaythrough" (fname (:progress-events @audio-state)) true)
    (.setAttribute elem "preload" "true")
    (.setAttribute elem "autobuffer" "true")
    (.setAttribute elem "src" path)
    (.pause elem)))

(defn disable-sound []
  (helper/console-log @audio-state)
  (doseq [track (:playing @audio-state)]
;    (pause (playing (track (:files @audio-state))))
))

(defn ended [name])

(defn play [name])

(defn pause [] (println "hi"))

(defn resume [])
