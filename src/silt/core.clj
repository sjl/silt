(ns silt.core
  (:require [lanterna.screen :as s]
            [roul.random :as rr]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:gen-class))


; Data ------------------------------------------------------------------------
(def fps 10)
(def world-width 600)
(def world-height 400)

(def pond-count 100)
(def pond-size 3)

(def screen (s/get-screen :swing))
(defonce running (atom true))
(def window-loc (ref [0 0]))
(defonce last-timestamp (atom 0))

(defonce terrain (ref []))
(def terrain-rate 2)
(def terrain-objects
  {{:name :rock :glyph "*"} 20
   {:name :shrub :glyph "%" :styles {:fg :green}
    :energy 0.1} 80})

(def landmarks
  (ref #{{:name :monolith :glyph "#" :loc [0 0] :styles {:fg :black :bg :yellow}}
         {:name :colossus :glyph "@" :loc [200 100] :styles {:fg :black :bg :red}}}))


(def world-temp (ref 0))
(def mutation-chance 1)

(def eve
  {:glyph "@"
   :styles {:fg :white}
   :temp 0
   :insulation 0
   :age 0
   :energy 100.0})


; Utils -----------------------------------------------------------------------
(defmacro while-let [binding-form & body]
  `(loop []
     (when-let ~binding-form
       ~@body
       (recur))))

(defn random-coord []
  [(rr/rand-int 0 world-width)
   (rr/rand-int 0 world-height)])

(defn normalize-coord [v limit]
  (cond
    (< v 0) (normalize-coord (+ limit v) limit)
    (>= v limit) (normalize-coord (- v limit) limit)
    :else v))

(defn normalize-world-coords [[wx wy]]
  [(normalize-coord wx world-width)
   (normalize-coord wy world-height)])

(defmacro maybe [chance value & body]
  `(fn [~value]
     (if (rr/rand-bool ~chance)
       (do ~@body)
       ~value)))


; World Generation ------------------------------------------------------------
(defn generate-terrain []
  (for [x (range world-width)
        y (range world-height)
        :when (rr/rand-bool terrain-rate)]
    (assoc (rr/rand-nth-weighted terrain-objects)
           :loc [x y])))

(defn generate-pond [[ox oy]]
  (for [_ (range 200)
            :let [x (rr/rand-gaussian-int ox (* pond-size 1.5))
                  y (rr/rand-gaussian-int oy pond-size)]]
    {:name :water
     :glyph "≈"
     :energy 0.1
     :loc (normalize-world-coords [x y])
     :styles {:fg :black :bg :blue}}))

(defn generate-water []
  (->> (for [_ (range pond-count)]
         (generate-pond (random-coord)))
    (into #{})
    (apply concat)))

(defn dedupe-terrain [terrain]
  (->> (for [{:keys [loc] :as t} terrain]
         [loc t])
    (into {})
    vals))

(defn reset-terrain! []
  (let [new-terrain (-> (generate-terrain)
                      (into (generate-water))
                      dedupe-terrain)]
    (dosync
      (ref-set terrain new-terrain))
    nil))


; Animals ---------------------------------------------------------------------
(defn can-reproduce [animal]
  (> (:energy animal) 50))

(defn clone [animal]
  (-> animal
    (assoc :age 0)
    (assoc :energy 50)
    (update-in [:insulation]
               (maybe mutation-chance v
                      (+ v (rand-nth [-1 1]))))
    (update-in [:styles :fg]
               (maybe mutation-chance v
                      (rr/rand-nth [:white :blue :green :yellow :red])))))

(defn reproduce [animal]
  [(update-in animal [:energy] #(- % 40))
   (clone animal)])


; Drawing ---------------------------------------------------------------------
(defn calc-screen-coords [[wx wy]]
  (let [[ox oy] @window-loc]
    (normalize-world-coords [(- wx ox) (- wy oy)])))

(defn draw-terrain! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} @terrain
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn draw-landmarks! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} @landmarks
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn from-right [screen n]
  (- (nth (s/get-size screen) 0) n))

(defn draw-screen! [screen]
  (letfn [(put-right [s y]
            (s/put-string screen (from-right screen (.length s)) y s))]
    (s/clear screen)
    (draw-terrain! screen)
    (draw-landmarks! screen)
    (put-right " SILT  " 0)
    (put-right (str @world-temp "° ") 1)
    (s/move-cursor screen (from-right screen 1) 0)
    (s/redraw screen)))


; Input -----------------------------------------------------------------------
(defn move-window!
  ([key] (move-window! key 1))
  ([key scale]
   (dosync
     (commute window-loc
              #(let [[x y] %1]
                 (case %2
                   (:up \k) [x (- y scale)]
                   (:down \j) [x (+ y scale)]
                   (:left \h) [(- x scale) y]
                   (:right \l) [(+ x scale) y]))
              key))))

(defn handle-input! [screen]
  (while-let [key (s/get-key screen)]
    (case key
      :escape (reset! running false)
      (:up :down :left :right) (move-window! key 10)
      (\h \j \k \l) (move-window! key)
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
  (dosync (ref-set world-temp 0))

  (reset-terrain!)

  (future (main-loop))

  )
