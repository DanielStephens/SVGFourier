(ns browser
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]))

(defonce mouse-coordinates (r/atom {:x 0 :y 0}))
(defonce anim-time (r/atom 0))

(def log js/console.log)

(defn vis
  ([s]
   [:p [:code (prn-str s)]])
  ([n s]
   [:p [:code n " : " (prn-str s)]]))

(defn vis-a
  ([s]
   [:p [:code (prn-str @s)]])
  ([n s]
   [:p [:code n " : " (prn-str @s)]]))

(defn elem [id]
  (js/document.getElementById id))

(defn v-op [op & vs]
  (apply merge-with op vs))

(def v+ (partial v-op +))
(def v- (partial v-op -))
(defn v' [v]
  (into {} (map (fn [[k d]] [k (- d)]) v)))
(defn v-mul [v d]
  (v-op *
        v
        (into {} (map (fn [[k _]] [k d]) v))))
(defn v-div [v d]
  (v-mul v (/ 1 d)))

(defn xy [elem-id event]
  (let [canvas (elem elem-id)
        rect (.getBoundingClientRect canvas)]
    {:x (/ (- (.-clientX event) (.-left rect))
           (.-width rect))
     :y (/ (- (.-clientY event) (.-top rect))
           (.-height rect))}))

(defn point-on-curve
  [^js/SVGGeometryElement curve-elem t]
  (let [p (.getPointAtLength curve-elem (* (.getTotalLength curve-elem) t))]
    {:x (.-x p)
     :y (.-y p)}))

(defn sample-curve
  [curve-elem samples]
  (v-div (->> (range samples)
              (map #(point-on-curve curve-elem (/ 1 %)))
              (apply v+))
         samples))

(defn point-on-bezier
  [{:keys [p1 c1 c2 p2]} t]
  (let [t' (- 1 t)]
    (v+ (v-mul p1 (* t' t' t'))
        (v-mul c1 (* 3 t' t' t))
        (v-mul c2 (* 3 t' t t))
        (v-mul p2 (* t t t)))))

(defn dist
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt
    (+ (* (- x2 x1) (- x2 x1))
       (* (- y2 y1) (- y2 y1)))))

(defn closest-point-on-bezier
  [bez p]
  (let [ep 0.05
        f #(dist p (point-on-bezier bez %))
        t (loop [ta 0 tb 1 c 0]
            (if (< (- tb ta) ep)
              (/ (+ tb ta) 2)
              (let [k (/ (+ tb ta) 2)
                    fa (f (- k ep))
                    fb (f (+ k ep))]
                (if (< fa fb)
                  (recur ta k (inc c))
                  (recur k tb (inc c))))))]
    (point-on-bezier bez t)))

(defn closest-point-on-curve
  [curve p]
  (->> curve
       (map-indexed (fn [i {:keys [p1 c1 c2 p2] :as bez}]
                      {:p p
                       :h (dist (v-div (v+ p1 c1 c2 p2) 4)
                                p)
                       :i i
                       :bez bez}))
       (sort-by :h)
       (take 4)
       (map (fn [{:keys [bez] :as b}]
              (let [cp (closest-point-on-bezier bez p)]
                (assoc b
                  :cp cp
                  :d (dist cp p)))))
       (sort-by :d)
       first))

(defn curve [points]
  (let [c (take (count points) (partition 4 1 (cycle points)))]
    (map (fn [[a b c d]]
           {:p1 b
            :c1 (v+ b (v-div (v- c a) 4))
            :c2 (v- c (v-div (v- d b) 4))
            :p2 c})
         c)))

(defn svg-curve [curve]
  (let [p-str #(str (:x %) " " (:y %))]
    (loop [s (str "M " (p-str (:p1 (first curve))) \newline)
           [{:keys [c1 c2 p2]} & more] curve]
      (if p2
        (recur (str s
                    "C "
                    (p-str c1) " "
                    (p-str c2) " "
                    (p-str p2) \newline)
               more)
        s))))

(defn node-opacity
  [p1 p2]
  (let [d 50
        o (/ (- d
                (dist p1 p2))
             d)]
    (max 0.1 (min 1
                  o))))

(defn bezier [width height curve-atom]
  (let [viewport (r/atom {:x 0 :y 0})
        points (r/atom [
                        (r/atom {:x (/ width 3) :y (/ height 3)})
                        (r/atom {:x (* 2 (/ width 3)) :y (/ height 3)})
                        ;(r/atom {:x 100 :y 100})
                        (r/atom {:x (/ width 3) :y (* 2 (/ height 3))})])
        mouse (fn [] (merge-with *
                                 {:x width :y height}
                                 @viewport))
        pxy (fn [{:keys [x y mouse-down]}]
              (if mouse-down
                (v+ {:x x :y y} (v- (mouse) mouse-down))
                {:x x :y y}))
        xys #(doall (map (comp pxy deref) @points))
        mouse-up (fn [p]
                   (js/window.addEventListener
                     "mouseup"
                     (fn [& _]
                       (swap! p #(-> %
                                     (dissoc :mouse-down)
                                     (merge (pxy @p)))))))
        make-point (fn [{:keys [i p]}]
                     (fn [& _]
                       (let [index (mod (+ 2 i) (count @points))
                             patom (r/atom (assoc p :mouse-down p))]
                         (swap! points
                                #(concat (take index %)
                                         [patom]
                                         (drop index %)))
                         (mouse-up patom))))
        fourier (r/atom {})]
    (fn [& _]
      (let [ps (xys)
            cur (curve ps)
            m (mouse)
            {:keys [cp] :as closest} (closest-point-on-curve cur m)]
        (reset! curve-atom (elem "drawn-path"))
        [:div
         [vis (mouse)]
         [vis-a fourier]
         [:svg {:id "canvas"
                :view-box (str "0 0 " width " " height)
                :on-mouse-move (fn [e] (reset! viewport (xy "canvas" e)))}
          [:rect {:width width
                  :height height
                  :opacity 0
                  :on-mouse-down (make-point closest)
                  :on-double-click (make-point closest)}]
          (->> (sort-by (comp Math/abs first) @fourier)
               (reductions (fn [[_ p1] [i p2]] [i (v+ p1 p2)]))
               (map (fn [[i p]]
                      [:ellipse {:key i
                                 :rx (/ 0.5 (Math/abs i)) :ry (/ 0.5 (Math/abs i))
                                 :fill "green"
                                 :cx (:x p)
                                 :cy (:y p)}]))
               (doall))
          [:path {:id "drawn-path"
                  :d
                  (svg-curve cur)
                  :stroke "black"
                  :fill "transparent"
                  :on-double-click (make-point closest)
                  :on-mouse-down (make-point closest)}]
          [:ellipse {:rx 2 :ry 2
                     :fill "red"
                     :cx (:x cp)
                     :cy (:y cp)
                     :on-double-click (make-point closest)
                     :on-mouse-down (make-point closest)}]
          (doall (map-indexed (fn [i p]
                                (let [{:keys [x y] :as pos} (pxy @p)
                                      id (str "node" i)]
                                  [:ellipse {:id id
                                             :key i
                                             :rx 2 :ry 2
                                             :fill "blue"
                                             :opacity (node-opacity pos (mouse))
                                             :cx x
                                             :cy y
                                             :on-double-click (fn [& _]
                                                                (swap! points #(concat (take i %) (drop (inc i) %))))
                                             :on-mouse-down (fn [& _]
                                                              (swap! p assoc :mouse-down (mouse))
                                                              (mouse-up p))
                                             }]))
                              @points))]]))))

(defn exp-vec
  [v]
  {:x (double (Math/cos v))
   :y (double (Math/sin v))})

(defn v*complex
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (- (* x1 x2) (* y1 y2))
   :y (+ (* y1 x2) (* x1 y2))})

(defn curve-fourier-const
  [curve-elem n]
  (let [samples 100
        points (->> (range samples)
                    (map (fn [i] (/ i samples)))
                    (map #(v*complex (exp-vec (* 2 n Math/PI %))
                                     (point-on-curve curve-elem %))))
        avg (v-div (apply v+ points) samples)]
    avg))

(defn curve-fourier
  [curve-elem depth]
  (when curve-elem
    (->> (range (- depth) (inc depth))
        (map (fn [n] [n (curve-fourier-const curve-elem n)]))
        (into {}))))

(defn step-fourier
  [fourier t]
  (let [t (mod t 1)
        rot (fn [n v] (v*complex (exp-vec (* 2 Math/PI n (- t)))
                                 v))]
    (->> fourier
         (map (fn [[n v]] [n (rot n v)]))
         (into {}))))

(defn fourier-position
  [fourier t]
  (let [f (step-fourier fourier t)]
    (->> (sort-by (comp Math/abs first) f)
         (reductions (fn [[_ p1] [i p2]] [i (v+ p1 p2)])))))

(defn fourier-path
  [fourier]
  (let [samples 100
        steps (->> (range samples)
                   (map #(/ % samples)))]
    (->> steps
         (map #(fourier-position fourier %))
         (map (comp second last)))))

(defn fourier [width height curve-atom]
  (let [fourier (r/atom {})
        path (r/atom [])
        update-fourier (fn [curve-elem]
                         (let [f (curve-fourier curve-elem 10)]
                           (reset! fourier f)
                           (reset! path (fourier-path f))))]
    (fn [& _]
      (let [
            fp (fourier-position @fourier
                                 0 #_(/ @anim-time 10000))
            [first-p :as p] @path]
        (js/setTimeout #(update-fourier @curve-atom) 1000)
        [:div
         [vis-a fourier]
         [vis-a curve-atom]
         [:svg {:id "canvas"
                :view-box (str "0 0 " width " " height)}
          [:path {:id "svg-path"
                  :d
                  (str "M "
                       (->> p
                            (map (fn [{:keys [x y]}] (str x " " y " L ")))
                            (str/join " "))
                       (str (:x first-p) " " (:y first-p)))
                  :stroke "black"
                  :fill "transparent"}]
          #_(let [[_ p] (last positions)]
              [:ellipse {:rx 0.5 :ry 0.5
                         :fill "red"
                         :cx (:x p)
                         :cy (:y p)}])
          (->> fp
               (map (fn [[i p]]
                      [:ellipse {:key i
                                 :rx (/ 10 (inc (Math/abs i))) :ry (/ 10 (inc (Math/abs i)))
                                 :fill "red"
                                 :cx (:x p)
                                 :cy (:y p)}]))
               (doall))]]))))

(def mouse-updater
  {:on-mouse-move (fn [event]
                    (reset! mouse-coordinates {:x (.-clientX event) :y (.-clientY event)})
                    nil)})

(defn animate [t]
  (reset! anim-time t)
  (.requestAnimationFrame js/window animate))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")
  (let [curve (r/atom [])]
    (rdom/render [:div
                  ;mouse-updater
                  ;[vis "mouse" mouse-coordinates]
                  ;[counting-component]
                  ;[timer-component]
                  ;[shared-state]
                  ;[ball-bounce-2]
                  [bezier 2000 300 curve]
                  [fourier 2000 300 curve]
                  ]
                 (js/document.getElementById "app")))
  (.requestAnimationFrame js/window animate))

(defn ^:dev/once init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
