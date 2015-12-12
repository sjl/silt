(ns silt.core
  (:require [lanterna.screen :as s]
            [roul.random :as rr]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:gen-class))


; Data ------------------------------------------------------------------------
(def fps 20)
(def world-width 600)
(def world-height 400)

(def screen (s/get-screen :swing))
(defonce running (atom true))
(def window-loc (ref [0 0]))
(defonce last-timestamp (atom 0))

(defonce terrain (ref []))
(def terrain-rate 1)
(def terrain-objects
  [{:name :rock :glyph "*"}
   {:name :shrub :glyph "%"}])

(def landmarks (ref
                 #{{:name :monolith :glyph "#" :loc [0 0] :styles {:fg :black :bg :yellow}}
                   {:name :colossus :glyph "@" :loc [200 100] :styles {:fg :black :bg :red}}
                   }))


; Utils -----------------------------------------------------------------------
(defmacro while-let [binding-form & body]
  `(loop []
     (when-let ~binding-form
       ~@body
       (recur))))


; World Generation ------------------------------------------------------------
(defn generate-terrain []
  (for [x (range world-width)
        y (range world-height)
        :when (rr/rand-bool terrain-rate)]
    (assoc (rr/rand-nth terrain-objects)
           :loc [x y])))

(defn reset-terrain! []
  (let [new-terrain (generate-terrain)]
    (dosync
      (ref-set terrain new-terrain))
    nil))


; Drawing ---------------------------------------------------------------------
(defn normalize-coord [v limit]
  (cond
    (< v 0) (normalize-coord (+ limit v) limit)
    (>= v limit) (normalize-coord (- v limit) limit)
    :else v))

(defn normalize-world-coords [[wx wy]]
  [(normalize-coord wx world-width)
   (normalize-coord wy world-height)])

(defn calc-screen-coords [[wx wy]]
  (let [[ox oy] @window-loc]
    (normalize-world-coords [(- wx ox) (- wy oy)])))

(defn draw-terrain! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph]} @terrain
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph))))

(defn draw-landmarks! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} @landmarks
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn draw-screen! [screen]
  (s/clear screen)
  (draw-terrain! screen)
  (draw-landmarks! screen)
  (s/put-string screen (- (nth (s/get-size screen) 0) 7) 0 " SILT ")
  (s/move-cursor screen (- (nth (s/get-size screen) 0) 6) 0)
  (s/redraw screen))


; Input -----------------------------------------------------------------------
(defn move-window! [key]
  (dosync
    (commute window-loc
             #(let [[x y] %1]
                (case %2
                  :up [x (dec y)]
                  :down [x (inc y)]
                  :left [(dec x) y]
                  :right [(inc x) y]))
             key)))

(defn handle-input! [screen]
  (while-let [key (s/get-key screen)]
    (case key
      :escape (reset! running false)
      (:up :down :left :right) (move-window! key)
      nil)))


; Game loop -------------------------------------------------------------------
(defn throttle! []
  (let [current (System/currentTimeMillis)
        sofar (- current @last-timestamp)
        desired (/ 1000 fps)
        wait (- desired sofar)]
    (reset! last-timestamp current)
    (if (> wait 0)
      (Thread/sleep wait))))

(defn main-loop []
  (s/in-screen
    screen
    (while @running
      (draw-screen! screen)
      (handle-input! screen)
      (throttle!)))
  (reset! running true))


; Scratch ---------------------------------------------------------------------
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (reset! running false)
  (reset! running true)

  (reset-terrain!)

  (future (main-loop))

  )
