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
(def insulation-cost 0.01)

(def screen-size (atom [0 0]))
(defn handle-resize [cols rows]
  (reset! screen-size [cols rows]))
(def screen (s/get-screen :swing {:resize-listener handle-resize}))

(def pond-count 100)
(def pond-size 3)
(def initial-energy 200.0)
(def hunger-rate 0.25)

(def fruit-energy 50)
(def fruit-rate 0.5)

(def paused (atom false))
(def dirty (atom true))

(def directions [[-1 -1] [ 0 -1] [ 1 -1]
                 [-1  0] [ 0  0] [ 1  0]
                 [-1  1] [ 0  1] [ 1  1]])

(defonce running (atom true))
(def window-loc (ref [0 0]))
(defonce last-timestamp (atom 0))
(def cursor-loc (ref [0 1]))

(defonce terrain (ref {}))
(def terrain-rate 0.8)

(def shrub {:name :shrub
            :glyph "%"
            :styles {:fg :green}
            :energy 0
            :solid true})
(def rock {:name :rock
           :glyph "*"
           :energy 0
           :solid true})
(def water {:name :water
            :glyph "≈"
            :energy 0.05
            :solid false
            :styles {:fg :black :bg :blue}})

(def terrain-objects
  {water 1 rock 20 shrub 80})


(def world-temp (ref 0))
(def clone-energy 100)
(def reproduction-rate 10)

(def animals (ref {}))
(def initial-animals 400)
(def eve
  {:glyph "@"
   :styles {:fg :white}
   :temp 0
   :insulation 0
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

(defmacro maybe-update-in [test-form m key-vec & body]
  `(if ~test-form
     (update-in ~m ~key-vec ~@body)
     ~m))

(defn dir-add [[x y] [dx dy]]
  (normalize-world-coords [(+ x dx) (+ y dy)]))

(defn neighbors [coord]
  (map (partial dir-add coord) directions))

(defn neighboring-things [thing things]
  (->> thing
    :loc
    neighbors
    (map things)
    (filter identity)))

(defn uuid []
  (str (java.util.UUID/randomUUID)))

(defn mutate-directions [dirs]
  (update-in dirs [(rr/rand-int 0 9) 1] inc))

(defn clamp [v minimum]
  (max v minimum))

(defn mutate-animal
  ([animal] (mutate-animal animal nil))
  ([animal mc]
   (-> animal
     (update :insulation
             (maybe (or mc 10) v
                    (clamp (+ v (rand-nth [-1 1])) 1)))
     (update :directions
             (maybe (or mc 20) v
                    (mutate-directions v)))
     (update-in [:styles :fg]
                (maybe (or mc 0.8) v
                       (rr/rand-nth [:white :blue :green :yellow :red])))
     (update :glyph
             (maybe (or mc 0.05) v
                    (rr/rand-nth [";" "☃" "$" "&" "!" ":" "¥" "£" "¤" "€"
                                  "‡" "ß" "¶" "µ" "¢" "¬" "¿" "?" "§" "@"]))))))

(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn to-loc-map [coll]
  (into {} (map (juxt :loc identity) coll)))

(defn to-loc-map-safe [coll]
  (loop [next-thing (first coll)
         remaining (rest coll)
         result (transient {})]
    (if next-thing
      (let [loc (:loc next-thing)]
        (if (result loc)
          (recur (assoc next-thing
                        :loc (dir-add loc (rand-nth directions)))
                 remaining
                 result)
          (recur (first remaining)
                 (rest remaining)
                 (assoc! result loc next-thing))))
      (persistent! result))))

(defn abs [n]
  ; eat shit, clojure
  (if (< n 0) (- n) n))


; Mysteries -------------------------------------------------------------------
(def landmarks
  (ref (to-loc-map
         #{{:name :monolith :loc [0 0]
            :glyph "#" :styles {:fg :black :bg :yellow}
            :description "A sleek, rectangular, octarine monolith.  What is its function?"
            :action
            (fn [self]
              (when (and (rr/rand-bool 0.1)
                         (empty? @animals))
                (ref-set animals {(:loc eve) eve})))}
           {:name :yggdrasil :loc (random-coord)
            :glyph "Y" :styles {:fg :white :bg :green}
            :description "An immense ash tree.  Its branches touch the stars."
            :action
            (fn [{[x y] :loc}]
              (when (rr/rand-bool 0.25)
                (let [dx (rr/rand-gaussian-int 0 10)
                      dy (rr/rand-gaussian-int 0 10)
                      target (normalize-world-coords [(+ x dx) (+ y dy)])]
                  (when-not (= [dx dy] [0 0])
                    (alter terrain assoc target (assoc shrub :loc target))))))}
           {:name :colossus :loc (random-coord)
            :glyph "@" :styles {:fg :black :bg :red}
            :description "A massive granite statue of an alien being."
            :action
            (fn [{[x y :as loc] :loc :as self}]
              (when (zero? (rem @day 1000))
                (let [dest (normalize-world-coords [(inc x) y])]
                  (alter landmarks dissoc loc)
                  (alter landmarks assoc dest (assoc self :loc dest)))))}
           {:name :fountain :loc (random-coord)
            :glyph "ƒ" :styles {:fg :white :bg :blue}
            :description "A marble fountain burbles peacefully."
            :action
            (fn [{:keys [loc]}]
              (doseq [animal (->> loc
                               neighbors
                               (map animals)
                               (filter identity))]
                (alter animals
                       #(update % (:loc animal) mutate-animal 100))))}
           })))


; Animals ---------------------------------------------------------------------
(defn can-reproduce [animal]
  (> (:energy animal) 150))

(defn clone [animal]
  (-> animal
    (assoc :id (uuid))
    (assoc :energy clone-energy)
    (update :loc (fn [[x y]] (normalize-world-coords [(inc x) y])))
    mutate-animal))

(defn reproduce [animal]
  [(update animal :energy - clone-energy)
   (clone animal)])

(defn try-move [orig dir]
  (if (= dir [0 0])
    orig
    (let [dest (dir-add orig dir)]
      (if (or (get-in @terrain [dest :solid])
              (contains? @landmarks dest)
              (contains? @animals dest))
        orig
        dest))))


(defn in-water [{:keys [loc]}]
  (= :water (get-in @terrain [loc :name])))

(defn affect-temp [animal]
  (assoc animal :temp
         (/ @world-temp
            (:insulation animal)
            (if (in-water animal) 5 1))))

(defn fix-temp [{:keys [temp] :as animal}]
  (-> animal
    (assoc :temp 0)
    (update :energy - (* 0.1 (abs temp)))))

(defn find-resources [animal]
  (let [found (->> (neighboring-things animal @terrain)
                (map :energy))]
    (update animal :energy (partial reduce +) found)))

(defn wander [animal]
  (update animal :loc
          try-move
          (rr/rand-nth-weighted (:directions animal))))

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
    try-reproduce))

(defn tick-animals [animals]
  (to-loc-map-safe (mapcat tick-animal (vals animals))))


; World Generation ------------------------------------------------------------
(defn grow-shrub [shrub]
  (assoc shrub
         :energy fruit-energy
         :styles {:fg :magenta}))

(defn strip-shrub [shrub]
  (assoc shrub
         :energy 0
         :styles {:fg :green}))

(defn tick-shrub [shrub]
  (if (zero? (:energy shrub))
    (if (rr/rand-bool fruit-rate)
      (grow-shrub shrub)
      shrub)
    (if (empty? (neighboring-things shrub @animals))
      shrub
      (strip-shrub shrub))))

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
    (assoc water :loc (normalize-world-coords [x y]))))

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

(defmacro loop-screen [sx sy wc & body]
  `(let [[swidth# sheight#] @screen-size]
    (doseq [~sx (range swidth#)
            ~sy (range sheight#)
            :let [~wc (calc-world-coords [~sx ~sy])]]
      ~@body)))

(defn draw-terrain! [screen]
  (loop-screen
    sx sy wc
    (when-let [thing (@terrain wc)]
      (s/put-string screen sx sy (:glyph thing) (:styles thing)))))

(defn draw-landmarks! [screen]
  (let [[swidth sheight] @screen-size]
    (doseq [{:keys [loc glyph styles]} (vals @landmarks)
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn draw-animals! [screen]
  (let [[swidth sheight] @screen-size]
    (doseq [{:keys [loc glyph styles]} (vals @animals)
            :let [[sx sy] (calc-screen-coords loc)]
            :when (and (< -1 sx swidth)
                       (< -1 sy sheight))]
      (s/put-string screen sx sy glyph styles))))

(defn from-right [screen n]
  (- (nth @screen-size 0) n))

(defn from-bottom [screen n]
  (- (nth @screen-size 1) n))

(defn draw-landmark-description! [screen]
  (when-let [lm (@landmarks (calc-world-coords @cursor-loc))]
    (s/put-string screen 0 0 (:description lm))))

(defn str-animal [a]
  (clojure.string/split-lines (with-out-str (clojure.pprint/pprint a))))

(defn draw-animal-stats! [screen]
  (when @paused
    (when-let [animal (@animals (calc-world-coords @cursor-loc))]
      (doseq [[i line] (map-indexed vector (str-animal animal))]
        (s/put-string screen 0 i line)))))

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
    (apply handle-resize (s/get-size screen))
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
  (set! *warn-on-reflection* true)
  (future (main-loop))

  )
