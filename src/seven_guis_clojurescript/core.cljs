(ns seven-guis-clojurescript.core
  (:require
   [clojure.walk :as walk]
   [clojure.string :as string]
   [cljs.reader :refer [read-string]]
   [reagent.core :as r]
   [reagent.dom :as d]))

;; -------------------------
;; Views
;; Each part of the 7GUIs challenge is contained in a single component.
;; This would not be ideal in most cases, but there is not much code for
;; each part so it felt easier to do it this way.


(def input-style
  {:text {:width "3rem"
          :outline "none"}
   :button {:margin-left "1rem"}})

(defn counter []
  (r/with-let [count (r/atom 0)]
    [:div
     [:input {:type "text"
              :readOnly true
              :value @count
              :style (:text input-style)}]
     [:button {:on-click #(swap! count inc)
               :style (:button input-style)}
      "Count"]]))

(defn temperature-converter []
  (r/with-let [temp (r/atom {:c 0 :f 32})
               f-to-c #(let [f (.. % -target -value)]
                         (swap! temp assoc :f f)
                         (when-not (js/isNaN f)
                           (swap! temp assoc :c (-> f (- 32) (* (/ 5 9))))))
               c-to-f #(let [c (.. % -target -value)]
                         (swap! temp assoc :c c)
                         (when-not (js/isNaN c)
                           (swap! temp assoc :f (-> c (* (/ 9 5)) (+ 32)))))]
    [:span
     [:input {:type "text"
              :value (:c @temp)
              :on-change c-to-f
              :style (:text input-style)}]
     " Celsius = "
     [:input {:type "text"
              :value (:f @temp)
              :on-change f-to-c
              :style (:text input-style)}]
     " Fahrenheit"]))

(def flight-style
  {:display "flex"
   :flex-direction "column"
   :justify-content "space-between"
   :width "10rem"
   :height "7.5rem"})

(defn flight-booker []
  (r/with-let [today (as-> (js/Date.) date
                       (str (.getFullYear date) "-"
                            (.padStart (str (inc (.getMonth date))) 2 "0") "-"
                            (.padStart (str (.getDate date)) 2 "0")))
               state (r/atom {:type "one-way flight" :start today :end ""})
               handle-change (fn [e k] (swap! state assoc k (.. e -target -value)))
               handle-submit (fn [e]
                               (.preventDefault e)
                               (if (= "one-way flight" (:type @state))
                                 (.alert js/window
                                         (str "You have booked a one-way flight on " (:start @state)))
                                 (.alert js/window
                                         (str "You have booked a return flight that flies out on "
                                              (:start @state) " and returns on " (:end @state)))))]
    [:form {:on-submit handle-submit
            :style flight-style}
     [:select {:value (:type @state)
               :on-change #(handle-change % :type)}
      [:option "one-way flight"]
      [:option "return flight"]]
     [:input {:type "date"
              :value (:start @state)
              :required true
              :min today
              :max (:end @state)
              :on-change #(handle-change % :start)}]
     [:input {:type "date"
              :value (:end @state)
              :required (= "return flight" (:type @state))
              :disabled (= "one-way flight" (:type @state))
              :min (:start @state)
              :on-change #(handle-change % :end)}]
     [:button "Book"]]))

(def timer-style
  {:display "flex"
   :flex-direction "column"
   :justify-content "space-between"
   :width "20rem"})

(defn timer []
  (let [state (r/atom {:timer nil :time 0 :duration 10})
        create-timer #(swap! state assoc :timer
                             (js/setInterval
                              (fn []
                                (swap! state update :time (fn [t]
                                                            (min (:duration @state) (+ 0.1 t))))) 100))
        clear-timer #(js/clearInterval (:timer @state))
        reset-time #(swap! state assoc :time 0)
        change-duration #(swap! state assoc :duration (js/parseFloat (.. % -target -value)))]
    (r/create-class
     {:display-name "timer"
      :component-did-mount create-timer
      :component-will-unmount clear-timer
      :reagent-render (fn [] [:div
                              [:div {:style timer-style}
                               [:span "Elapsed Time: "
                                [:progress {:max (:duration @state)
                                            :value (:time @state)}]]
                               [:span (str (.toFixed (:time @state) 1) "s")]
                               [:span "Duration: "
                                [:input {:type "range"
                                         :value (:duration @state)
                                         :on-change change-duration
                                         :min 10
                                         :max 60
                                         :step 0.1}]]
                               [:button {:on-click reset-time} "Reset"]]])})))

(def crud-style
  {:grid {:display "grid"
          :grid-template-columns "1fr 1fr 1fr"
          :gap "1rem"}
   :filter {:grid-column "1 / 2"
            :grid-row "1 / 2"
            :white-space "nowrap"}
   :name-labels {:grid-column "2 / 3"
                 :grid-row "2 / 3"}
   :name {:grid-column "3 / 4"
          :grid-row "2 / 3"}
   :buttons {:grid-column "1 / 3"
             :grid-row "3 / 4"
             :display "flex"
             :justify-content "space-between"
             :width "60%"}
   :select {:grid-column "1 / 2"
            :grid-row "2 / 3"}})

(defn crud []
  (let [state (r/atom {:people {}
                       :key nil
                       :name ""
                       :surname ""
                       :filter ""})
        reset #(swap! state assoc :key nil :name "" :surname "")
        create (fn []
                 (swap! state update :people assoc (str (random-uuid))
                        {:name (:name @state) :surname (:surname @state)})
                 (reset))
        read #(let [key (.. % -target -value)
                    {:keys [name surname]} (get-in @state [:people key])]
                (swap! state assoc :key key :name name :surname surname))
        update (fn []
                 (swap! state assoc-in [:people (:key @state)]
                        {:name (:name @state) :surname (:surname @state)})
                 (reset))
        delete (fn []
                 (swap! state update-in [:people] dissoc (:key @state))
                 (reset))
        filter #(reduce-kv (fn [m k {:keys [name surname]}]
                             (if (or (= "" (:filter @state))
                                     (string/starts-with? surname (:filter @state)))
                               (conj m [k (str surname ", " name)])
                               m))
                           []
                           (:people @state))
        handle-filter #(swap! state assoc :filter (.. % -target -value))
        handle-name #(swap! state assoc :name (.. % -target -value))
        handle-surname #(swap! state assoc :surname (.. % -target -value))]
    (fn []
      [:div
       [:div {:style (:grid crud-style)}
        [:div {:style (:filter crud-style)}
         [:label "Filter prefix: "]
         [:input {:type "text" :value (:filter @state) :on-change handle-filter}]]
        (into [:select {:on-change read :on-focus read :size "6" :style (:select crud-style)}]
              (for [[k v] (filter)] [:option {:value k} v]))
        [:div {:style (:name-labels crud-style)}
         [:label "Name: "]
         [:label "Surname: "]]
        [:div {:style (:name crud-style)}
         [:input {:type "text"  :value (:name @state) :on-change handle-name}]
         [:input {:type "text" :value (:surname @state) :on-change handle-surname}]]
        [:div {:style (:buttons crud-style)}
         [:button {:disabled (some? (:key @state))
                   :on-click create}
          "Create"]
         [:button {:disabled (nil? (:key @state))
                   :on-click update}
          "Update"]
         [:button {:disabled (nil? (:key @state))
                   :on-click delete}
          "Delete"]]]])))

(def circle-style
  {:svg {:align-self "center"
         :margin-bottom "25px"
         :width "60rem"
         :height "500px"
         :border "1px solid black"}
   :modal {:width "60rem"
           :border-radius "0.25rem"
           :border "1px solid black"
           :box-shadow "0 19px 38px rgba(0,0,0,0.30), 0 15px 12px"
           :margin-bottom "25px"}
   :modal-button {:width "1.5rem"
                  :height "1.5rem"
                  :border "none"
                  :margin-left "0.25rem"}
   :modal-content {:display "flex"
                   :flex-direction "column"
                   :padding "1rem"}})

;; TODO: should also check for change when resizing since it now pushes a change to the undo stack event if there isn't one.
(defn circle-drawer []
  (let [state (r/atom {:circles {}
                       :editing nil
                       :edit-menu? false
                       :edit? false
                       :modal-x nil
                       :modal-y nil
                       :tmp-resize-change nil
                       :edit-message ""
                       :undo '()
                       :redo '()})
        close-modal #(when (some? (:editing @state))
                       (swap! state update :undo conj (:tmp-resize-change @state))
                       (swap! state update-in [:circles (:editing @state)] assoc :fill "transparent")
                       (swap! state assoc :edit-menu? false :edit? false :editing nil))
        undo #(let [[op uid circle] (first (:undo @state))
                    curr-circle (get-in @state [:circles uid])]
                (close-modal)
                (swap! state update :undo rest)
                (case op
                  :remove (do (swap! state update :circles dissoc uid)
                              (swap! state update :redo conj [:add uid circle]))
                  :resize (do (swap! state update :circles assoc uid circle)
                              (swap! state update :redo conj [:resize uid curr-circle]))))
        redo #(let [[op uid circle] (first (:redo @state))
                    curr-cicle (get-in @state [:circles uid])]
                (close-modal)
                (swap! state update :redo rest)
                (case op
                  :add (do (swap! state update :circles assoc uid circle)
                           (swap! state update :undo conj [:remove uid circle]))
                  :resize (do (swap! state update :circles assoc uid circle)
                              (swap! state update :undo conj [:resize uid curr-cicle]))))
        edit #(swap! state update-in [:circles (:editing @state)] assoc :r (int (.. % -target -value)))
        handle-click-svg (fn [e]
                           (when (= "svg" (.. e -target -nodeName))
                             (let [rect (.getBoundingClientRect (.. e -target))
                                   uid (str (random-uuid))
                                   circle {:x (int (- (.-clientX e) (.-left rect)))
                                           :y (int (- (.-clientY e) (.-top rect)))
                                           :r 20
                                           :fill "transparent"}]
                               (close-modal)
                               (swap! state update :undo conj [:remove uid circle])
                               (swap! state update :circles assoc uid circle))))
        handle-click-circle (fn [uid]
                              (let [{:keys [x y] :as circle} (get-in @state [:circles uid])]
                                (close-modal)
                                (swap! state update-in [:circles uid] assoc :fill "rgb(228,233,237)")
                                (swap! state assoc
                                       :tmp-resize-change [:resize uid circle]
                                       :edit-message (str "Adjust diameter of circle at (" x ", " y ").")
                                       :editing uid
                                       :edit-menu? true
                                       :modal-x x
                                       :modal-y y)))]
    (fn []
      [:div {:style {:display "flex" :flex-direction "column" :justify-content "space-between" :height "650px"}}
       [:div {:style {:align-self "center" :display "flex" :justify-content "space-between" :width "7rem"}}
        [:button {:on-click undo :disabled (empty? (:undo @state))} "Undo"]
        [:button {:on-click redo :disabled (empty? (:redo @state))} "Redo"]]
       ;TODO: Since svg renders the elements in the order they're recieved there 
       ;will be cases when completely overlapped circles can't be clicked. 
       ;Im not sure how to fix this without instead just finding the closest 
       ;circle that the pointer is inside.
       (-> (reduce-kv (fn [m k {:keys [x y r fill]}]
                        (conj m [:circle {:on-click (fn [e]
                                                      (.stopPropagation e)
                                                      (handle-click-circle k))
                                          :cx x :cy y :r r :stroke "black" :fill fill :cursor "pointer"}]))
                      []
                      (:circles @state))
           (conj [:g {:style {:visibility (if (:edit-menu? @state) "visible" "hidden") :cursor "pointer"}
                      :on-click #(swap! state assoc :edit-menu? false :edit? true)}
                  [:rect {:x (:modal-x @state) :y (:modal-y @state) :width 95 :height 15 :stroke-width 0.5 :stroke "black" :fill "gray" :fill-opacity 0.1}]
                  [:text {:x (+ 3 (:modal-x @state)) :y (+ 12 (:modal-y @state)) :font-size 10} "Adjust diamater..."]])
           (->> (into [:svg {:on-click handle-click-svg :style (:svg circle-style)}])))
       [:div {:style (merge (:modal circle-style) {:visibility (if (:edit? @state) "visible" "hidden")})}
        [:button {:on-click close-modal
                  :style (:modal-button circle-style)} "X"]
        [:div {:style (:modal-content circle-style)}
         [:input {:type "range"
                  :value (get-in @state [:circles (:editing @state) :r])
                  :on-change edit
                  :min 10
                  :max 90
                  :step 1}]
         [:span (:edit-message @state)]]]])))

(def cells-style
  {:container {:width "1200px" :height "650px" :overflow "auto"}
   :table {:border-collapse "collapse"}
   :table-element {:border "1px solid black"
                   :height "2rem"}
   :input {:border "none"
           :height "100%"}})

;; TODO: When a cell is empty another cells dependent on that one should return WAT?
(defn cells []
  (let [letters (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        state (r/atom (->> (for [l letters
                                 n (range 100)]
                             [(str l n) {:formula "" :value ""}])
                           (into {})))
        resolve-fn (fn [sym args]
                     (case sym
                       SUM (apply + args)
                       SUB (apply - args)
                       DIV (apply / args)
                       MUL (apply * args)
                       AVG (/ (apply + args) (count args))))
        deref-or-str #(if (string? %) % (deref %))
        resolve-val (fn [val]
                      (if (re-matches #"[A-Z][0-9]{1,2}" (str val))
                        (deref-or-str (get-in @state [(str val) :value]))
                        val))
        parse (fn [s & _]
                (cond
                  (= "" s) ""
                  (not (js/isNaN s)) (js/parseFloat s)
                  (= "=" (first s)) (try (walk/postwalk (fn [x]
                                                          (if (seq? x)
                                                            (resolve-fn (first x) (rest x))
                                                            (resolve-val x))) (read-string (subs s 1)))
                                         (catch js/Error e (do (js/console.log (ex-cause e)) "WAT?")))
                  :else s))
        handle-focus (fn [e k] (set! (.. e -target -value) (get-in @state [k :formula])))
        handle-blur (fn [e k]
                      (if (= (get-in @state [k :formula]) (.. e -target -value))
                        (set! (.. e -target -value) nil)
                        (let [cells (re-seq #"[A-Z][0-9]{1,2}" (.. e -target -value))]
                          (swap! state update k assoc
                                 :formula (.. e -target -value)
                                 :value (r/track parse (.. e -target -value) (map #(@(r/cursor state [k %])) cells)))
                          (set! (.. e -target -value) nil))))]
    (fn []
      [:div
       [:span "An Expression is entered in the form of =(expr*). Expressions can be nested, 
               but only the first parentheses should be prepended with '='.
               Valid formulas are SUM, SUB, DIV, MUL and AVG."]
       [:div {:style (:container cells-style)}
        [:table {:style (:table cells-style)}
         [:colgroup {:span (count letters)}]
         [:thead
          [:tr
           [:th ""]
           (doall (map (fn [letter] [:th {:key (str "heading-" letter)} letter]) letters))]]
         [:tbody
          (doall
           (map (fn [c]
                  [:tr {:key (str "row-" c)} [:td c]
                   (doall
                    (map (fn [v]
                           [:td {:key (str "td-" v c) :style (:table-element cells-style)}
                            [:input {:type "text" :style (:input cells-style)
                                     :key (str "input-" v c)
                                     :placeholder @(r/track deref-or-str @(r/cursor state [(str v c) :value]))
                                     :on-focus #(handle-focus % (str v c))
                                     :on-blur #(handle-blur % (str v c))}]])
                         letters))]) (range 100)))]]]])))

(def card-style
  {:display "flex"
   :flex-wrap "wrap"
   :justify-content "center"
   :align-items "center"
   :width "8rem"
   :height "5rem"
   :border-radius "0.25rem"
   :cursor "pointer"
   :padding "0.5rem"})

(def home-style
  {:container {:display "flex"
               :flex-direction "column"
               :align-items "center"
               :margin-top "-70px" ;yuck, but yeah
               }
   :description {:line-height 1}
   :menu {:display "flex"
          :justify-content "space-between"
          :width "1200px"
          :margin-bottom "2rem"}})

(defn card
  [{:keys [on-click active]} title]
  [:div {:on-click on-click :style (merge card-style
                                          {:border (if active
                                                     "2px solid rgb(0,153,255)"
                                                     "1px solid black")})}
   title])

(defn home-page []
  (let [selected-panel (r/atom "Counter")
        panels '("Counter" "Temperature Converter" "Flight Booker" "Timer" "CRUD" "Circle Drawer" "Cells")
        change-panel #(reset! selected-panel %)
        panel? #(= % @selected-panel)]
    (fn []
      [:div {:style (:container home-style)}
       [:h1 "7GUIs"]
       [:h6 {:style (:description home-style)} "An implementation of "
        [:a {:href "https://eugenkiss.github.io/7guis/"} "the 7GUIs benchmark "]
        "written in clojurescript/reagent. The source code is on "
        [:a {:href "https://github.com/antonmoller/seven_guis_clojurescript"} "GitHub."]]
       (into [:div {:style (:menu home-style)}]
             (for [panel panels]
               [card {:on-click #(change-panel panel) :active (panel? panel)} panel]))
       [(case @selected-panel
          "Counter" counter
          "Temperature Converter" temperature-converter
          "Flight Booker" flight-booker
          "Timer" timer
          "CRUD" crud
          "Circle Drawer" circle-drawer
          "Cells" cells)]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))