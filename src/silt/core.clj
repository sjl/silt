(ns silt.core
  (:require [lanterna.screen :as s]
            [roul.random :as rr]
            [clojure.pprint]
            [clojure.core.match :refer [match]]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:gen-class))


; Data ------------------------------------------------------------------------
(def fps 50)
(def day (ref 0))
(def world-width 600)
(def world-height 400)
(def tick-delay (atom 50))
(def age-effect 300)
(def insulation-cost 0.01)

(def pond-count 100)
(def pond-size 3)
(def initial-energy 200.0)
(def hunger-rate 0.1)

(def fruit-energy 40)
(def fruit-rate 1)

(def paused (atom false))
(def dirty (atom true))

(def directions [[-1 -1] [ 0 -1] [ 1 -1]
                 [-1  0] [ 0  0] [ 1  0]
                 [-1  1] [ 0  1] [ 1  1]])

(def screen (s/get-screen :swing))
(defonce running (atom true))
(def window-loc (ref [0 0]))
(defonce last-timestamp (atom 0))
(def cursor-loc (ref [0 1]))

(defonce terrain (ref {}))
(def terrain-rate 1)
(def terrain-objects
  {{:name :rock :glyph "*" :energy 0} 20
   {:name :shrub :glyph "%" :styles {:fg :green} :energy 0} 80})


(def world-temp (ref 0))
(def mutation-chance 10)
(def reproduction-rate 10)

(def animals (ref {}))
(def initial-animals 400)
(def eve
  {:glyph "@"
   :styles {:fg :white}
   :temp 0
   :insulation 0
   :age 0
   :energy initial-energy
   :id "eve"
   :directions [[[-1 -1] 2] [[0 -1] 1] [[1 -1] 2]
                [[-1  0] 2] [[0  0] 0] [[1  0] 2]
                [[-1  1] 2] [[0  1] 1] [[1  1] 2]]
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

(defn manhattan [[x y] [x1 y1]]
  (+ (Math/abs (- x x1))
     (Math/abs (- y y1))))

(defmacro maybe-update-in [test-form m key-vec & body]
  `(if ~test-form
     (update-in ~m ~key-vec ~@body)
     ~m))

(defn dir-add [[x y] [dx dy]]
  (normalize-world-coords [(+ x dx) (+ y dy)]))

(defn neighbors [coord]
  (map (partial dir-add coord) directions))

(defn uuid []
  (str (java.util.UUID/randomUUID)))

(defn mutate-directions [dirs]
  (update-in dirs [(rr/rand-int 0 9) 1] inc))

(defn mutate-animal [animal mc]
  (-> animal
    (update :insulation
            (maybe mc v
                   (+ v (rand-nth [-1 1]))))
    (update :directions
            (maybe mc v
                   (mutate-directions v)))
    (update-in [:styles :fg]
               (maybe mc v
                      (rr/rand-nth [:white :blue :green :yellow :red])))))

(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))


; Mysteries -------------------------------------------------------------------
(def landmarks
  (ref {[0 0]
        {:name :monolith :glyph "#" :loc [0 0] :styles {:fg :black :bg :yellow}
         :description "A sleek, rectangular, octarine monolith.  What is its function?"
         :action (fn [self]
                   (when (and (rr/rand-bool 0.1)
                              (empty? @animals))
                     (ref-set animals {(:loc eve) eve})))}
        [200 100]
        {:name :colossus :glyph "@" :loc [200 100] :styles {:fg :black :bg :red}
         :description "A massive granite statue of a being.  You do not recognize the species."
         :action identity}

        [299 350]
        {:name :fountain :glyph "ƒ" :loc [299 350] :styles {:fg :white :bg :blue}
         :description "A marble fountain burbles peacefully."
         :action (fn [{:keys [loc]}]
                   (doseq [animal (->> loc
                                    neighbors
                                    (map animals)
                                    (filter identity))]
                     (dosync
                       (alter animals
                              #(update % (:loc animal) mutate-animal 100)))))}
        }))


; Animals ---------------------------------------------------------------------
(defn can-reproduce [animal]
  (> (:energy animal) 100))

(defn clone [animal]
  (-> animal
    (assoc :id (uuid))
    (assoc :age 0)
    (assoc :energy 60)
    (update :loc (fn [[x y]] (normalize-world-coords [(inc x) y])))
    (mutate-animal mutation-chance)))

(defn reproduce [animal]
  [(update animal :energy - 60)
   (clone animal)])

(defn try-move [orig dir]
  (let [dest (dir-add orig dir)]
    (if (contains? @terrain dest)
      orig
      dest)))


(defn affect-temp [animal]
  (assoc animal :temp (float (/ (Math/abs @world-temp)
                                (inc (Math/abs (:insulation animal)))))))

(defn fix-temp [{:keys [temp] :as animal}]
  (-> animal
    (assoc :temp 0)
    (update :energy - (* 0.1 (Math/abs temp)))))

(defn find-resources [{:keys [loc] :as animal}]
  (let [found (->> loc
                neighbors
                (map terrain)
                (filter identity)
                (map :energy)
                (reduce +))]
    (update animal :energy + found)))

(defn wander [animal]
  (update animal :loc
          try-move
          (rr/rand-nth-weighted (:directions animal))))

(defn age [animal]
  (let [{:keys [age] :as animal} (update animal :age inc)]
    (if (and (> age 50)
             (rr/rand-bool (inc (/ (:age animal) age-effect))))
      []
      [animal])))

(defn hunger [{:keys [insulation] :as animal}]
  (update animal :energy - (+ hunger-rate
                              (* insulation insulation-cost))))

(defn starve [animal]
  (if (< (:energy animal) 0)
    []
    [animal]))

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
    wander
    find-resources
    hunger
    starve
    ; age
    try-reproduce))

(defn tick-animals [animals]
  (into {} (map (juxt :loc identity)
                (mapcat tick-animal (vals animals)))))


; World Generation ------------------------------------------------------------
(defn tick-shrub [shrub]
  (if (rr/rand-bool fruit-rate)
    (-> shrub
      (assoc-in [:energy] fruit-energy)
      (assoc-in [:styles :fg] :magenta))
    (let [animals @animals]
      (if (->> shrub
            :loc
            neighbors
            (map animals)
            (filter identity)
            empty?)
        shrub
        (-> shrub
          (assoc-in [:energy] 0)
          (assoc-in [:styles :fg] :green))))))

(defn tick-terrain [terrain]
  (into {} (for [[loc {:keys [name] :as obj}] terrain]
             [loc (case name
                    :shrub (tick-shrub obj)
                    obj)])))


(defn dedupe-things [things]
  (into {} (map (juxt :loc identity) things)))


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
     :energy 0.01
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
            (assoc :energy initial-energy)
            (assoc :loc (random-coord))))
        eve))


(defn reset-window! []
  (dosync (ref-set window-loc [0 0])))

(defn reset-terrain! []
  (let [new-terrain (as-> (generate-terrain) t
                      (into t (generate-water))
                      (dedupe-things t))]
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

(defn calc-world-coords [[sx sy]]
  (let [[ox oy] @window-loc]
    (normalize-world-coords [(+ sx ox) (+ sy oy)])))

(defn draw-terrain! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} (vals @terrain)
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn draw-landmarks! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} (vals @landmarks)
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn draw-animals! [screen]
  (let [[swidth sheight] (s/get-size screen)]
    (doseq [{:keys [loc glyph styles]} (vals @animals)
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn from-right [screen n]
  (- (nth (s/get-size screen) 0) n))

(defn from-bottom [screen n]
  (- (nth (s/get-size screen) 1) n))

(defn draw-landmark-description! [screen]
  (when-let [lm (@landmarks (calc-world-coords @cursor-loc))]
    (s/put-string screen 0 0 (:description lm))))

(defn str-animal [a]
  (clojure.string/split-lines (with-out-str (clojure.pprint/pprint a))))

(defn draw-animal-stats! [screen]
  (when-let [animal (@animals (calc-world-coords @cursor-loc))]
    (doseq [[i line] (map-indexed vector (str-animal animal))]
      (s/put-string screen 0 i line))))

(defn draw-screen! [screen]
  (when @dirty
    (reset! dirty false)
    (letfn [(put-right [s y]
              (s/put-string screen (from-right screen (.length s)) y s))]
      (s/clear screen)
      (draw-terrain! screen)
      (draw-animals! screen)
      (draw-landmarks! screen)
      (draw-landmark-description! screen)
      (draw-animal-stats! screen)
      (s/put-string screen 0 (from-bottom screen 1) (str " " @day))
      (put-right " SILT  " 0)
      (put-right (str @world-temp "° ") 1)
      (put-right (str (count @animals) "  ") 2)
      (put-right
        (let [[x y] @window-loc] (str x " " y " "))
        (from-bottom screen 1))
      (apply s/move-cursor screen @cursor-loc)
      (s/redraw screen))))


; Input -----------------------------------------------------------------------
(defn move-cursor! [key]
  (dosync
    (commute cursor-loc
             #(let [[x y] %1]
                (case %2
                  (\w \k) [x (- y 1)]
                  (\s \j) [x (+ y 1)]
                  (\a \h) [(- x 1) y]
                  (\d \l) [(+ x 1) y]))
             key))
  (mark-dirty!))

(defn move-window!
  ([key] (move-window! key 1))
  ([key scale]
   (dosync
     (commute window-loc
              #(normalize-world-coords
                 (let [[x y] %1]
                   (case %2
                     :up [x (- y scale)]
                     :down [x (+ y scale)]
                     :left [(- x scale) y]
                     :right [(+ x scale) y])))
              key))
   (mark-dirty!)))

(defn reset-day! []
  (dosync (ref-set day 0)))

(defn reset-temperature! []
  (dosync (ref-set world-temp 0)))

(defn reset-tick-delay! []
  (reset! tick-delay 50))


(defn update-terrain! []
  (alter terrain tick-terrain))

(defn update-animals! []
  (alter animals tick-animals))

(defn update-landmarks! []
  (doseq [lm (vals @landmarks)]
    ((:action lm) lm)))

(defn update-world! []
  (dosync
    (update-animals!)
    (update-terrain!)
    (update-landmarks!)
    (commute day inc))
  (mark-dirty!))


(defn update-temperature! [amt]
  (dosync (commute world-temp + amt))
  (mark-dirty!))

(defn reset-world! []
  (reset-temperature!)
  (reset-tick-delay!)
  (reset-day!)
  (reset-window!)
  (reset-terrain!)
  (reset-animals!)
  (mark-dirty!))

(defn toggle-pause! []
  (swap! paused not))

(defn update-tick-delay! [amt]
  (swap! tick-delay + amt))

(defn handle-input! [screen]
  (while-let [key (s/get-key screen)]
    (case key
      :escape
      (reset! running false)

      (:up :down :left :right)
      (move-window! key 10)

      (\h \j \k \l \w \a \s \d)
      (move-cursor! key)

      \space
      (toggle-pause!)

      \1
      (update-world!)

      (\+ \=)
      (update-temperature! 1)

      (\- \_)
      (update-temperature! -1)

      \R
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

(defn tick []
  (Thread/sleep @tick-delay)
  (when (not @paused)
    (update-world!)))

(defn tick-loop []
  (while @running
    (tick)))


(defn main-loop []
  (reset! running true)
  (reset! paused false)
  (reset-world!)
  (future (tick-loop))
  (s/in-screen
    screen
    (while @running
      (draw-screen! screen)
      (handle-input! screen)
      (throttle!))))


; Scratch ---------------------------------------------------------------------
(defn -main [& args]
  (main-loop))

(comment
  (reset! running false)
  (reset! running true)

  (reset-window!)
  (reset-terrain!)
  (reset-animals!)

  (future (main-loop))

  )
