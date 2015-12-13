(ns silt.core
  (:require [lanterna.screen :as s]
            [roul.random :as rr]
            [clojure.core.match :refer [match]]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:gen-class))


; Data ------------------------------------------------------------------------
(def fps 50)
(def world-width 600)
(def world-height 400)

(def pond-count 100)
(def pond-size 3)

(def dirty (atom true))

(def directions [[-1 -1] [ 0 -1] [ 1 -1]
                 [-1  0] [ 0  0] [ 1  0]
                 [-1  1] [ 0  1] [ 1  1]])

(def screen (s/get-screen :swing))
(defonce running (atom true))
(def window-loc (ref [0 0]))
(defonce last-timestamp (atom 0))

(defonce terrain (ref {}))
(def terrain-rate 2)
(def terrain-objects
  {{:name :rock :glyph "*"} 20
   {:name :shrub :glyph "%" :styles {:fg :green}
    :energy 0.1} 80})

(def landmarks
  (ref #{{:name :monolith :glyph "#" :loc [0 0] :styles {:fg :black :bg :yellow}}
         {:name :colossus :glyph "@" :loc [200 100] :styles {:fg :black :bg :red}}}))


(def world-temp (ref 0))
(def mutation-chance 10)
(def reproduction-rate 5)

(def animals (ref []))
(def initial-animals 400)
(def eve
  {:glyph "@"
   :styles {:fg :white}
   :temp 0
   :insulation 0
   :age 0
   :energy 100.0
   :id "eve"
   :loc [0 1]
   })


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

(defn uuid [] (str (java.util.UUID/randomUUID)))

; Animals ---------------------------------------------------------------------
(defn can-reproduce [animal]
  (> (:energy animal) 50))

(defn clone [animal]
  (-> animal
    (assoc :id (uuid))
    (assoc :age 0)
    (assoc :energy 50)
    (update-in [:loc] (fn [[x y]] (normalize-world-coords [(inc x) y])))
    (update-in [:insulation]
               (maybe mutation-chance v
                      (+ v (rand-nth [-1 1]))))
    (update-in [:styles :fg]
               (maybe mutation-chance v
                      (rr/rand-nth [:white :blue :green :yellow :red])))))

(defn reproduce [animal]
  [(update-in animal [:energy] #(- % 40))
   (clone animal)])

(defn try-move [[x y :as orig] [dx dy]]
  (let [dest (normalize-world-coords [(+ x dx) (+ y dy)])]
    (if (contains? @terrain dest)
      orig
      dest)))


(defn affect-temp [animal] animal)
(defn fix-temp [animal] animal)
(defn find-resources [animal]
  animal)

(defn wander [animal]
  (update-in animal [:loc]
             try-move
             (rr/rand-nth directions)))

(defn age [animal]
  (let [{:keys [age] :as animal} (update-in animal [:age] inc)]
    (if (and (> age 50)
             (rr/rand-bool (inc (/ (:age animal) 100))))
      []
      [animal])))

(defn try-reproduce [animals]
  (match animals
    [] []
    [animal] (if (and (can-reproduce animal)
                      (rr/rand-bool reproduction-rate))
               (reproduce animal)
               animals)))

(defn tick-animal [animal]
  (-> animal
    affect-temp
    fix-temp
    find-resources
    wander
    age
    try-reproduce))

(defn tick-animals [animals]
  (vec (mapcat tick-animal animals)))


; World Generation ------------------------------------------------------------
(defn dedupe-things [things]
  (->> (for [{:keys [loc] :as t} things]
         [loc t])
    (into {})
    vals))


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


(defn generate-animals []
  (conj (for [_ (range initial-animals)]
          (-> eve
            clone
            (assoc :energy 100)
            (assoc :loc (random-coord))))
        eve))


(defn reset-window! []
  (dosync (ref-set window-loc [0 0])))

(defn reset-terrain! []
  (let [new-terrain (as-> (generate-terrain) t
                      (into t (generate-water))
                      (dedupe-things t)
                      (map (juxt :loc identity) t)
                      (into {} t))]
    (dosync
      (ref-set terrain new-terrain))
    nil))

(defn reset-animals! []
  (let [new-animals (dedupe-things (generate-animals))]
    (dosync
      (ref-set animals new-animals))
    nil))


; Drawing ---------------------------------------------------------------------
(defn mark-dirty! []
  (reset! dirty true))

(defn calc-screen-coords [[wx wy]]
  (let [[ox oy] @window-loc]
    (normalize-world-coords [(- wx ox) (- wy oy)])))

(defn draw-terrain! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} (vals @terrain)
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

(defn draw-animals! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} @animals
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn from-right [screen n]
  (- (nth (s/get-size screen) 0) n))

(defn draw-screen! [screen]
  (when @dirty
    (reset! dirty false)
    (letfn [(put-right [s y]
              (s/put-string screen (from-right screen (.length s)) y s))]
      (s/clear screen)
      (draw-terrain! screen)
      (draw-animals! screen)
      (draw-landmarks! screen)
      (put-right " SILT  " 0)
      (put-right (str @world-temp "° ") 1)
      (put-right (str (count @animals) "  ") 2)
      (s/move-cursor screen (from-right screen 1) 0)
      (s/redraw screen))))


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
              key))
   (mark-dirty!)))

(defn update-animals! [key]
  (dosync
    (alter animals
           #(nth (iterate tick-animals %)
                 (case key
                   \1 1
                   \2 10
                   \3 100
                   \4 1000
                   \5 10000
                   \6 10000
                   \7 10000
                   \8 10000
                   \9 10000))))
  (mark-dirty!))

(defn update-temperature! [amt]
  (dosync (commute world-temp + amt))
  (mark-dirty!))

(defn reset-world! []
  (reset-window!)
  (reset-terrain!)
  (reset-animals!)
  (mark-dirty!))

(defn handle-input! [screen]
  (while-let [key (s/get-key screen)]
    (case key
      :escape
      (reset! running false)

      (:up :down :left :right)
      (move-window! key 10)

      (\h \j \k \l)
      (move-window! key)

      \space
      (update-animals! \1)

      (\1 \2 \3 \4 \5 \6 \7 \8 \9)
      (update-animals! key)

      (\+ \=)
      (update-temperature! 1)

      (\- \_)
      (update-temperature! -1)

      \r
      (reset-world!)

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

  (reset-window!)
  (reset-terrain!)
  (reset-animals!)

  (future (main-loop))

  )
