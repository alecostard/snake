(ns acs.snake.app)

(defn make-point [x y] {:x x :y y})

(def canvas (js/document.getElementById "game-canvas"))

(def width (.-width canvas))

(def height (.-height canvas))

(def ctx (.getContext canvas "2d"))

(def food-rate 0.05)

(def initial-state {:snake (list [(/ width 2) (/ height 2)])
                    :size 3
                    :direction :n
                    :food #{}
                    :running true})

(def state (atom initial-state))

(defn wrap [pos] (mapv mod pos [width height]))

(def direction->delta {:e [1 0] :n [0 -1] :w [-1 0] :s [0 1]})

(defn update-snake [{:keys [snake direction size food] :as state}]
  (let [delta (direction->delta direction)
        snake-head (first snake)
        new-head (wrap (mapv + snake-head delta))
        new-snake (take size (conj snake new-head))]
    (cond
      (some #{new-head} snake)
      (assoc state :running false)
      
      (contains? food new-head)
      (-> state
          (assoc :snake new-snake)
          (update :size inc)
          (update :food #(disj % new-head)))

      :else
      (assoc state :snake new-snake))))

(defn should-spawn-food? [] (< (rand) food-rate))

(defn update-food [state]
  (if-not (should-spawn-food?)
    state
    (update state :food conj [(rand-int width) (rand-int height)])))

(defn update-state [state]
  (-> state update-snake update-food))

;;;; controls
(def opposed-direction {:e :w :w :e :n :s :s :n})

(defn set-direction [direction]
  (when-not (= (:direction @state) (opposed-direction direction))
    (swap! state update :direction (constantly direction))))

(def key->direction
  {"ArrowRight" :e "ArrowUp" :n "ArrowLeft" :w "ArrowDown" :s})

(defn keydown-handler [e]
  (when-let [dir (key->direction (.-key e))]
    (set-direction dir)))

;;;; display
(defn clear-canvas []
  (.clearRect ctx 0 0 width height))

(defn draw-pixel [[x y]]
  (set! (.-fillStyle ctx) "black")
  (.fillRect ctx x y 1 1))

(defn draw-snake [snake]
  (doall (map draw-pixel snake)))

(defn draw-food [food]
  (doall (map draw-pixel food)))

(defn draw-state [state]
  (clear-canvas)
  (draw-snake (:snake state))
  (draw-food (:food state)))

;;;; basic game functions
(defn run-game []
  (when (:running @state)
    (draw-state @state)
    (swap! state update-state)))

(defn init []
  (js/window.setInterval run-game 100)
  (js/document.addEventListener "keydown" keydown-handler false))