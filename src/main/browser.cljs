(ns browser
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]))

(defonce anim-time (r/atom 0))

;; JS UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def log js/console.log)

(defn on-event
  [event-id f]
  (js/window.addEventListener
    event-id
    f))

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

;; VEC UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn exp-vec
  [v]
  {:x (double (Math/cos v))
   :y (double (Math/sin v))})

(defn v*complex
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (- (* x1 x2) (* y1 y2))
   :y (+ (* y1 x2) (* x1 y2))})

(defn dist
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt
    (+ (* (- x2 x1) (- x2 x1))
       (* (- y2 y1) (- y2 y1)))))

(defn lerp
  [v1 v2 t]
  (v+ v1 (v-mul (v- v2 v1) t)))

;; VIEWPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn client-viewport []
  {:width js/document.documentElement.clientWidth
   :height js/document.documentElement.clientHeight})

(defn viewport []
  (merge (client-viewport)
         {:x 0
          :y 0
          :zoom 1}))

(defn svg-viewport
  [{:keys [x y zoom width height]}]
  (let [vwidth (* 900 zoom)
        vheight (* (/ vwidth
                      width)
                   (- height 100)
                   2)
        vx (- x (/ vwidth 2))
        vy (- y (/ vheight 2))]
    {:x vx
     :y vy
     :width vwidth
     :height vheight
     :viewbox (str vx " " vy " " vwidth " " vheight)
     :zoom zoom
     :ui-scalar (/ 1 zoom)}))

(defn rel-mouse-xy [elem-id p]
  (let [canvas (elem elem-id)
        rect (.getBoundingClientRect canvas)]
    {:x (/ (- (:x p) (.-left rect))
           (.-width rect))
     :y (/ (- (:y p) (.-top rect))
           (.-height rect))}))

(defn denormalise-01 [{:keys [x y width height]} v]
  (v+ {:x (* width (:x v)) :y (* height (:y v))}
      {:x x :y y}))

(defn zoom
  [mx my mz viewport-atom]
  (let [dz (/ mz -30)
        half {:x 0.5 :y 0.5}
        in-canvas (rel-mouse-xy "canvas" {:x mx :y my})
        in-fourier (rel-mouse-xy "fourier" {:x mx :y my})
        from-canvas (dist half in-canvas)
        from-fourier (dist half in-fourier)
        offset (denormalise-01 (svg-viewport @viewport-atom)
                               (if (< from-canvas from-fourier)
                                 in-canvas
                                 in-fourier))]
    (swap! viewport-atom
           (fn [{:keys [x y zoom] :as vp}]
             (merge vp
                    {:zoom (+ zoom dz)}
                    (lerp {:x x :y y} offset (- dz)))))))

;; BEZIER UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; BEZIER CREATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-opacity
  [scale p1 p2]
  (let [d (* scale 500)
        o (/ (- d
                (dist p1 p2))
             d)]
    (max 0.1 (min 1
                  o))))

(defn bezier [viewport curve-atom]
  (let [svg-vp (svg-viewport @viewport)
        mouse-pos-01 (r/atom {:x 0 :y 0})
        verts (r/atom [(r/atom (denormalise-01 svg-vp {:x 0.4 :y 0.4}))
                       (r/atom (denormalise-01 svg-vp {:x 0.6 :y 0.4}))
                       (r/atom (denormalise-01 svg-vp {:x 0.6 :y 0.6}))])]
    (fn [viewport _]
      @viewport
      (let [{:keys [x y width height viewbox ui-scalar] :as svg-vp} (svg-viewport @viewport)
            mouse-pos (fn [] (denormalise-01 svg-vp @mouse-pos-01))
            vert-xy (fn [{:keys [x y mouse-down]}]
                      (if mouse-down
                        (v+ {:x x :y y} (v- (mouse-pos) mouse-down))
                        {:x x :y y}))
            vert-xys (doall (map (comp vert-xy deref) @verts))
            handle-mouse-up (fn [vert-atom]
                              (on-event "mouseup"
                                        (fn [& _]
                                          (swap! vert-atom #(-> %
                                                                (dissoc :mouse-down)
                                                                (merge (vert-xy @vert-atom)))))))
            make-vert (fn [{:keys [i p]}]
                        (fn [& _]
                          (let [index (mod (+ 2 i) (count @verts))
                                new-vert (r/atom (assoc p :mouse-down p))]
                            (swap! verts
                                   #(concat (take index %)
                                            [new-vert]
                                            (drop index %)))
                            (handle-mouse-up new-vert))))
            cur (curve vert-xys)
            m (mouse-pos)
            {:keys [cp] :as closest} (closest-point-on-curve cur m)]
        (reset! curve-atom {:element (elem "drawn-path")
                            :curve cur})
        [:svg {:id "canvas"
               :view-box viewbox
               :on-mouse-move (fn [e] (reset! mouse-pos-01 (rel-mouse-xy "canvas"
                                                                         {:x (.-clientX e) :y (.-clientY e)})))}
         [:rect {:x x :y y
                 :width width :height height
                 :opacity 0
                 :on-mouse-down (make-vert closest)
                 :on-double-click (make-vert closest)}]
         [:path {:id "drawn-path"
                 :d (svg-curve cur)
                 :stroke "#D8D2E1"
                 :stroke-width (* ui-scalar 6)
                 :fill "transparent"
                 :on-double-click (make-vert closest)
                 :on-mouse-down (make-vert closest)}]
         [:circle {:r (* ui-scalar 10)
                   :fill "#FFBF46"
                   :cx (:x cp)
                   :cy (:y cp)
                   :on-double-click (make-vert closest)
                   :on-mouse-down (make-vert closest)}]
         (doall (map-indexed (fn [i p]
                               (let [{:keys [x y] :as pos} (vert-xy @p)]
                                 [:circle {:key i
                                           :r (* ui-scalar 10)
                                           :fill "#B88E8D"
                                           :opacity (node-opacity ui-scalar pos (mouse-pos))
                                           :cx x
                                           :cy y
                                           :on-double-click (fn [& _]
                                                              (swap! verts #(concat (take i %) (drop (inc i) %))))
                                           :on-mouse-down (fn [& _]
                                                            (swap! p assoc :mouse-down (mouse-pos))
                                                            (handle-mouse-up p))
                                           }]))
                             @verts))]))))

;; FOURIER TRANSFORMATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn progress-fourier
  [fourier t]
  (let [t (mod t 1)
        rot (fn [n v] (v*complex (exp-vec (* 2 Math/PI n (- t)))
                                 v))]
    (->> fourier
         (map (fn [[n v]] [n (rot n v)]))
         (into {}))))

(defn fourier-position
  [fourier t]
  (let [f (progress-fourier fourier t)]
    (->> (sort-by (comp Math/abs first) f)
         (reductions (fn [[_ p1] [i p2]] [i (v+ p1 p2)])))))

(defn fourier-paths
  [fourier]
  (let [samples 1000
        f (memoize (fn [i] (fourier-position fourier (/ i samples))))]
    (fn [t]
      (f (int (* t samples))))))

(defn fourier-outer-path
  [path-fn]
  (let [samples 100]
    (->> (range samples)
         (map #(path-fn (/ % samples)))
         (map (comp second last)))))

(defn svg-path
  [[f :as path]]
  (str "M "
       (->> path
            (map (fn [{:keys [x y]}] (str x " " y " L ")))
            (str/join " "))
       (str (:x f) " " (:y f))))

(defn refine-fourier [state-atom element depth-steps max-depth]
  (let [{:keys [depth fourier]
         :or {depth -1}} @state-atom
        next-depth (min (+ depth depth-steps) max-depth)
        steps (range (inc depth) (inc next-depth))
        next-fourier (->> steps
                          (mapcat (fn [n] [[n (curve-fourier-const element n)]
                                           [(- n) (curve-fourier-const element (- n))]]))
                          (into {}))
        f (merge fourier next-fourier)
        position-fn (fourier-paths f)
        path (fourier-outer-path position-fn)]
    (swap! state-atom assoc
           :fourier f
           :depth next-depth
           :position-fn position-fn
           :path path
           :svg-path (svg-path path))))

(defn update-fourier
  [{:keys [element curve]} state-atom depth-steps max-depth]
  (when (not= (:curve @state-atom) curve)
    (swap! state-atom merge
           {:element element
            :curve curve
            :depth -1}))
  (when element
    (refine-fourier state-atom element depth-steps max-depth)))

(defn fourier-display [_viewport curve-atom _depth-steps _max-depth]
  (let [state (r/atom {})]
    (fn [viewport _curve-atom depth-steps max-depth]
      (let [{:keys [viewbox ui-scalar]} (svg-viewport @viewport)
            {:keys [position-fn svg-path]} @state
            fp (when position-fn (position-fn (/ @anim-time 10000)))]
        (update-fourier @curve-atom
                        state depth-steps max-depth)
        [:svg {:id "fourier"
               :view-box viewbox}
         (->> fp
              (partition 2 1)
              (map (fn [[[i centre] [_ p]]]
                     (let [d (dist centre p)]
                       [:ellipse {:key i
                                  :rx d
                                  :ry d
                                  :fill "transparent"
                                  :stroke "#D8D2E1"
                                  :opacity 0.5
                                  :stroke-dasharray "2, 2"
                                  :cx (:x centre)
                                  :cy (:y centre)}])))
              (doall))
         [:path {:id "svg-path"
                 :d svg-path
                 :stroke "#D8D2E1"
                 :stroke-width (* ui-scalar 6)
                 :fill "transparent"}]
         (->> fp
              drop-last
              (map (fn [[i p]]
                     [:ellipse {:key i
                                :rx (* ui-scalar (/ 10 (inc (Math/abs i))))
                                :ry (* ui-scalar (/ 10 (inc (Math/abs i))))
                                :fill "#B88E8D"
                                :cx (:x p)
                                :cy (:y p)}]))
              (doall))
         (when-let [[i p] (last fp)]
           [:ellipse {:key i
                      :rx (* ui-scalar 10) :ry (* ui-scalar 10)
                      :fill "#FFBF46"
                      :cx (:x p)
                      :cy (:y p)}])]))))

(defn animate [t]
  (reset! anim-time t)
  (.requestAnimationFrame js/window animate))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")
  (let [curve (r/atom [])
        viewport (r/atom (assoc (viewport) :zoom 1))]
    (set! js/document.body.onresize (fn [_]
                                      (swap! viewport merge (client-viewport))))
    (on-event "wheel"
              (fn [e]
                (let [x (.-clientX e)
                      y (.-clientY e)
                      zoom-delta (.-deltaY e)]
                  (zoom x y zoom-delta viewport))))
    (rdom/render [:div
                  [vis-a viewport]
                  [:div {:className "rowC"}
                   [bezier
                    viewport
                    curve]
                   [fourier-display
                    viewport
                    curve 2 10]]]
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
