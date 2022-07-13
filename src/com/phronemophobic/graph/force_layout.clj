(ns com.phronemophobic.graph.force-layout
  (:require [membrane.ui :as ui]
            [membrane.skia :as backend]
            [membrane.component :as com
             :refer [defui defeffect make-app] ]
            [membrane.basic-components :as basic]
            [ubergraph.core :as uber]
            [clojure.math.combinatorics :as combo]
            [euclidean.math.vector :as v]
            [euclidean.math.quaternion :as q]
            [clojure.core.async :as async]
            )
  (:import java.util.UUID)
  (:gen-class))

(def node-size 30)
(def node-radius (/ node-size 2))
(def attract-length (* node-size 5))

(def zaxis (v/vector 0 0 1))
(def zquarternion (q/from-angle-axis (/ Math/PI 2)
                                     zaxis))
(def attractk 0.01 )
(def connectk 0.05 )
(def repelk (* attract-length attract-length))

(defn attractive-force [p1 p2]
  (let [edge (v/sub p2 p1)
        magnitude (- (v/magnitude edge) attract-length )
        force (v/scale (v/normalize edge)
                       (* attractk magnitude))]
    force))

(defn connected-force [p1 p2]
  (let [edge (v/sub p2 p1)
        magnitude (- (v/magnitude edge) attract-length )
        force (v/scale (v/normalize edge)
                       (* connectk magnitude))]
    force))

(defn repel-force [p1 p2]
  (let [edge (v/sub p2 p1)
        dist (v/magnitude edge)
        magnitude (- (/ repelk
                        (* dist dist)))
        force (v/scale (v/normalize edge)
                       magnitude)]
    force))

(defn uuid []
  (java.util.UUID/randomUUID))

(defn add-node [g pos]
  (let [id (uuid)
        [x y] pos]
    (-> g
        (uber/add-nodes id)
        (uber/add-attrs id
                        {:pos (v/vector x y)}))))

(defn initial-state []
  {:g (uber/graph)
   :tool :add})

(defonce state (atom (initial-state)))

(defn init! []
  (reset! state (initial-state)))



(defeffect ::reset []
  (dispatch! :update nil
             (fn [old-state]
               ;; preserve window info
               (assoc (initial-state)
                      :winfo (:winfo old-state)))))

(defn jiggle [g]
  (transduce
   (comp
    (remove (fn [[n1 n2]]
              (= n1 n2)))
    (map
     (fn [[n1 n2]]
       (let [p1 (uber/attr g n1 :pos)
             p2 (uber/attr g n2 :pos)
             force (repel-force p1 p2)]
         [n1 (v/add (repel-force p1 p2)
                    (attractive-force p1 p2)
                    
                    (if (uber/has-edge? g n1 n2)
                      (connected-force p1 p2)
                      (v/vector 0 0)))]))))
   
   (completing
    (fn
      ([g [node force]]
       (uber/add-attr g node :pos (v/add force
                                         (uber/attr g node :pos))))))
   g
   
   (combo/cartesian-product (uber/nodes g)
                            (uber/nodes g))))

(defn jiggles []
  (when-not (:jiggling? @state)
    (future
      (try
       (when-let [repaint (-> @state
                              :winfo
                              ::backend/repaint)]

         (swap! state assoc :jiggling? true)
         (loop []
           (swap! state update :g jiggle)
           (repaint)
           (Thread/sleep 30)
           (when (:jiggling? @state)
             (recur))))))))

(defn stop-jiggling []
  (swap! state assoc :jiggling? false))

(defn rotate-pi [v pis]
  (q/rotate (q/from-angle-axis (* Math/PI pis)
                               zaxis)
            (v/vector (v/get-x v)
                      (v/get-y v)
                      0)))

(defn arrow
  ([p]
   (let [x (v/get-x p)
         y (v/get-y p)

         arrow1 (-> p
                    v/normalize
                    (rotate-pi 3/4)
                    (v/scale 15))

         arrow2 (-> p
                    v/normalize
                    (rotate-pi -3/4)
                    (v/scale 15))]
     (ui/with-style :membrane.ui/style-stroke
       [(ui/path [0 0]
                 [x y])
        (ui/translate x y
                      [(ui/path [0 0]
                                [(v/get-x arrow1)
                                 (v/get-y arrow1)])
                       (ui/path [0 0]
                                [(v/get-x arrow2)
                                 (v/get-y arrow2)])])]))))

(defeffect ::add-node [$g pos]
  (dispatch! :update $g add-node pos))

(defeffect ::jiggle [$g]
  (dispatch! :update $g jiggle))

(defeffect ::jiggles [jiggling?]
  (if jiggling?
    (jiggles)
    (stop-jiggling)))

(defeffect ::connect-node [$g n1 n2]
  (when (not= n1 n2)
   (dispatch! :update $g uber/add-directed-edges [n1 n2] )))

(defn closest-node [g pt]
  (apply min-key (fn [node]
                   (Math/abs (v/magnitude (v/sub pt (uber/attr g node :pos)))))
         (uber/nodes g)))


(defn ensure-magnitude [v m]
  (if (< (v/magnitude v)
         m)
    (v/scale (v/normalize v) m)
    v))

(defui gview [{:keys [g tool jiggling? mouse-temp]}]
  (let [connect-temp (get extra ::conect-extra)]
    (ui/wrap-on
     :mouse-down
     (fn [handler pos]
       (let [intents (handler pos)]
         (if (seq intents)
           intents
           
           (case tool
             :add
             [[::add-node $g pos]]

             :connect
             (let [[x y] pos
                   pt (v/vector x y)]
               (if connect-temp
                 [[::connect-node $g connect-temp (closest-node g pt)]
                  [:delete $connect-temp nil]]
                 [[:set $connect-temp (closest-node g pt)]]))

             ;; else
             (prn "unknown tool" tool)))))

     :mouse-move
     (fn [handler [mx my :as pos]]
       (into []
             cat
             [(handler pos)
              [[:set $mouse-temp (v/vector mx my)]]]))
     
     
     [(ui/spacer 1000 1000)

      ;; show arrow while connecting
      (when (and mouse-temp connect-temp)
        (let [pos (uber/attr g connect-temp :pos)]
          (ui/translate (v/get-x pos) (v/get-y pos)
                        (arrow (v/sub mouse-temp pos)))))
      
      (ui/vertical-layout
       (ui/horizontal-layout
        (basic/button {:text "reset"
                       :on-click
                       (fn []
                         [[::reset]])})
        (basic/button {:text "jiggle"
                       :on-click
                       (fn []
                         [[::jiggle $g]])})
        (ui/translate 5 5
                      (ui/on
                       ::basic/toggle
                       (fn [& args]
                         [[::jiggles (not jiggling?)]])
                       (basic/checkbox {:checked? jiggling?})))
        (ui/on
         ::basic/select
         (fn [& args]
           [(into [::basic/select] args)
            [:delete $connect-temp]])
         (basic/dropdown {:selected tool
                          :options [[:add "add"]
                                    [:connect "connect"]]}))))
      #_(into []
              (comp
               (remove (fn [[n1 n2]]
                         (= n1 n2)))
               (map
                (fn [[n1 n2]]
                  (let [p1 (-> (uber/attrs g n1)
                               :pos)
                        p2 (-> (uber/attrs g n2)
                               :pos)
                        force (attractive-force p1 p2)]
                    (ui/translate (v/get-x p1)
                                  (v/get-y p1)
                                  (arrow
                                   (ensure-magnitude force node-size)))))))
              (combo/cartesian-product (uber/nodes g)
                                       (uber/nodes g)))


      #_(into []
              (comp
               (remove (fn [[n1 n2]]
                         (= n1 n2)))
               (map
                (fn [[n1 n2]]
                  (let [p1 (-> (uber/attrs g n1)
                               :pos)
                        p2 (-> (uber/attrs g n2)
                               :pos)
                        force (repel-force p1 p2)]
                    (ui/translate (v/get-x p1)
                                  (v/get-y p1)
                                  (arrow (ensure-magnitude force node-size)))))))
              (combo/cartesian-product (uber/nodes g)
                                       (uber/nodes g)))

      (into []
            (map (fn [[n1 n2]]
                   (let [
                         p1 (uber/attr g n1 :pos)
                         p2 (uber/attr g n2 :pos)]
                     (ui/translate (v/get-x p1)
                                   (v/get-y p1)
                                   (arrow (v/sub p2 p1))))))
            (uber/edges g))

      (into []
            (map (fn [node]
                   (let [pos (-> (uber/attrs g node)
                                 :pos)
                         x (v/get-x pos)
                         y (v/get-y pos)]
                     (ui/translate (- x node-radius) (- y node-radius)
                                   (ui/with-style :membrane.ui/style-stroke
                                     (ui/rounded-rectangle node-size node-size
                                                           node-radius))))))
            (uber/nodes g))])))

(defn show! []
  (let [winfo (backend/run (make-app #'gview state))]
    (swap! state assoc :winfo winfo)))

(defn -main [& args]
  (swap! state assoc
         :winfo {::backend/repaint #'backend/glfw-post-empty-event})
  (backend/run-sync (make-app #'gview state)))





