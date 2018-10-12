(ns kundel.component
  (:require
    [reagent.core :as r]
    [debux.cs.core :refer-macros [clog dbg break]]
    [dommy.core :as dom]
    [kundel.css :as css]))

(def attrs (atom {}))

(defn ctor-attrs [this]
  (reset! attrs (assoc @attrs this {:narration (r/atom nil)
                                    :keyframe (r/atom nil)
                                    :current (r/atom nil)
                                    :timeout (r/atom nil)
                                    :instance-id (gensym "narration-instance-")
                                    :id (r/atom (gensym ""))})))

(defn has-attrs [this]
  (contains? @attrs this))

(defn narration [this] 
  (:narration (get @attrs this)))

(defn keyframe [this]
  (:keyframe (get @attrs this)))

(defn current [this]
  (:current (get @attrs this)))

(defn timeout [this]
  (:timeout (get @attrs this)))

(defn id [this]
  (:id (get @attrs this)))

(defn instance-id [this]
  (:instance-id (get @attrs this)))

;;
;; narration / timeline variables and utilities
;;

(defn get-narration
  "Give sections as per component contract returns linked structure {:section ?? :subsection ?? :flow ?? :subsnext ?? :subsprev ??} for each flow from each section and each subsection; in order."
  ([sections]
   (get-narration
     sections
     (:flows (first sections))
     (:subsections (first sections))
     (:flows (first (:subsections (first sections))))
     nil))
  ([sects flows subs subflows subsprev]
   (if (seq flows)
     (cons {:section (first sects) :subsection nil :flow (first flows) :subsnext nil :subsprev nil :numsubs (count subs)}
           (get-narration sects (rest flows) subs subflows subsprev))
     (if (seq subflows)
       (cons {:section (first sects) :subsection (first subs) :flow (first subflows) :subsnext (second subs) :subsprev subsprev :numsubs (count subs)}
             (get-narration sects nil subs (rest subflows) subsprev))
       (if (seq subs)
         (get-narration sects nil (rest subs) (:flows (second subs)) (first subs))
         (when (seq sects)
           (get-narration (rest sects))))))))

(defn get-narration-keyframe [narration flow]
  "retrieves narration keyframe for flow."
  (first (keep-indexed #(when (= flow (:flow %2)) %1) narration)))

(defn get-subsection-index-percentage-tuple [narration flow]
  "Retrieve 0-based index of flow in subsection, if any, or nil.  Retrieve percentage of all subsections where subsection occurs, so 3rd subsection of 4 subsections is at 75%.  These two values are in a tuple (index, percentage)"
  (let [_keyframe (get-narration-keyframe narration flow)
        _current (nth narration _keyframe)]
    (when-let [subsection (:subsection _current)]
      (let [subscoll (:subsections (:section _current))
            index (first (keep-indexed #(when (= subsection %2) %1) subscoll))]
        (list index (* 100 (/ index (count subscoll))))))))

(defn get-element-id [flow]
  "Retrieve HTML ID for 'flow' element"
  (str "narrator-flow-" (:id flow)))

(defn find-parent-with-class [js-element with-class]
  "Find a JS element that's a parent of passed in 'js-element' with class 'with-class'."
  (when js-element
    (when-let [parent (dom/parent js-element)]
      (if (dom/has-class? parent with-class)
        parent
        (find-parent-with-class parent with-class)))))

(defn html-collection-to-vec
  "Converts nodelist to (not lazy) seq."
  [hc]
  (into [] (for [k (range (aget hc "length"))]
             (aget hc k))))

(defn find-child-with-class [js-element with-class]
  "Find a JS element that's an immediate child of passed in 'js-element' with class 'with-class'."
  (when js-element
    (when-let [children (html-collection-to-vec (dom/children js-element))]
      (first
        (filter #(if (dom/has-class? % with-class)
                     %
                     nil)
                children)))))

(defn playing? [this]
  (not (nil? @(timeout this))))

(defn add-remove-classes-and-properties-for-animation [this]
  "go through all IDs and animate as required"
  (let [current-flow (:flow @(current this))
        current-section (:section @(current this))
        current-subsection (:subsection @(current this))
        current-flow-js-element (dom/sel1 (str "#" (get-element-id current-flow)))
        section-js-element (find-parent-with-class current-flow-js-element "narrator-section")
        sections-js-element (find-parent-with-class current-flow-js-element "narrator-sections")]
    (if (playing? this)
      (dom/add-class! sections-js-element "narrating")
      (dom/remove-class! sections-js-element "narrating"))
    (doseq [rec @(narration this)
            :let [flow (:flow rec)
                  flow-js-element (dom/sel1 (str "#"(get-element-id flow)))
                  subsection (:subsection rec)
                  section (:section rec)]
            :when flow-js-element
            :when (not= rec @(current this))]
      (dom/remove-class! flow-js-element "narrator-current")
      (when (and subsection (not (= subsection current-subsection)))
        (when-let [subsection-js-element (find-parent-with-class flow-js-element "narrator-subsection")]
          (dom/remove-class! subsection-js-element "narrator-current"))
        (when-let [subsection-frame-js-element (find-parent-with-class flow-js-element "narrator-subsection-frame")]
          (dom/remove-class! subsection-frame-js-element "narrator-current")
          (dom/remove-class! subsection-frame-js-element "has-previous-subsection")
          (dom/remove-class! subsection-frame-js-element "has-next-subsection")))
      (when (and section (not (= section current-section)))
        (when-let [section-js-element (find-parent-with-class flow-js-element "narrator-section")]
          (dom/remove-class! section-js-element "narrator-current")
          (dom/remove-class! section-js-element "narrating-in-subsection"))))
    (when current-flow-js-element
      (dom/add-class! current-flow-js-element "narrator-current")
      (when-let [subsection-js-element (find-parent-with-class current-flow-js-element "narrator-subsection")]
        (dom/add-class! subsection-js-element "narrator-current"))
      (when-let [subsection-frame-js-element (find-parent-with-class current-flow-js-element "narrator-subsection-frame")]
        (dom/add-class! subsection-frame-js-element "narrator-current")
        (let [[_ percentage] (get-subsection-index-percentage-tuple @(narration this) current-flow)
              carousel-js-element (find-parent-with-class current-flow-js-element "narrator-susbection-carousel")]
          (dom/set-style! carousel-js-element :transform (str "translateX(-" percentage "%)")))
        (when (:subsprev @(current this))
          (dom/add-class! subsection-frame-js-element "has-previous-subsection"))
        (when (:subsnext @(current this))
          (dom/add-class! subsection-frame-js-element "has-next-subsection")))
      (when sections-js-element
        (dom/add-class! section-js-element "narrator-current")
        (if current-subsection
          (dom/add-class! section-js-element "narrating-in-subsection")
          (dom/remove-class! section-js-element "narrating-in-subsection"))
        (when-let [subsection-frame-js-child (find-child-with-class section-js-element "narrator-subsection-frame")]
          (when (> (:numsubs @(current this)) 1)
            (dom/add-class! subsection-frame-js-child "has-next-subsection")))))))

(defn fire-event [this id]
  "Dispatch 'timeline' events."
  (let [event (.createEvent js/document "Event")]
    (.initEvent event "timeline" true true)
    (aset event "id" id)
    (aset event "playing" (playing? this))
    (.dispatchEvent this event)))

(defn start-progress-bar [this seconds]
  (.log js/console "start-progress-bar")
  (let [progress-parent (dom/sel1 (str "#narrator-buttons-" (instance-id this)))
        progress-bar (dom/sel1 (str "#narrator-buttons-progress" (instance-id this)))
        new-progress-bar (dom/create-element :div)]
    (when progress-bar
      (.log js/console "start-progress-bar remove old")
      (dom/remove! progress-bar))
    (.log js/console (str "start-progress-bar progress-parent:" progress-parent "progress-bar:" progress-bar "id:" (instance-id this) "new-progress-bar:" new-progress-bar))
    (dom/add-class! new-progress-bar (str "narrator-buttons-progress"))
    (dom/set-style! new-progress-bar :display "block")
    (dom/set-style! new-progress-bar :animation (str "narration-progress " seconds "s linear"))
    (dom/append! progress-parent new-progress-bar)
    (dom/set-attr! new-progress-bar :id (str "narrator-buttons-progress" (instance-id this)))))


(defn stop-progress-bar [this]
  (.log js/console "stop-progress-bar")
  (let [progress-bar (dom/sel1 (str "#narrator-buttons-progress" (instance-id this)))]
    (when progress-bar
      (.log js/console (str "stop-progress-bar remove old id:" (instance-id this)))
      (dom/remove! progress-bar))))

(defn stop-playing [this]
  (when (playing? this)
    (stop-progress-bar this)
    (js/clearTimeout @(timeout this))
    (reset! (timeout this) nil)
    (add-remove-classes-and-properties-for-animation this)))

(defn set-keyframe [this new-keyframe]
  (cond
    (< new-keyframe 0)
    (do
      (stop-playing this)
      (reset! (keyframe this) 0)
      (reset! (current this) (first @(narration this)))
      (fire-event this (:id (:flow @(current this)))))
    (>= new-keyframe (count @(narration this)))
    (do
      (stop-playing this)
      (reset! (keyframe this) (- (count @(narration this)) 1))
      (reset! (current this) (last @(narration this)))
      (fire-event this (:id (:flow @(current this)))))
    :else
    (do
      (reset! (keyframe this) new-keyframe)
      (reset! (current this) (nth @(narration this) new-keyframe))
      (fire-event this (:id (:flow @(current this))))))
  (add-remove-classes-and-properties-for-animation this))

(defn start-playing [this]
  (stop-playing this)
  (let [seconds (:seconds (:flow (nth @(narration this) @(keyframe this))))
        millis (* 1000 seconds)]
    (start-progress-bar this seconds)
    (reset! (timeout this) (js/setTimeout #(do
                                             (when (playing? this)
                                               (set-keyframe this (+ 1 @(keyframe this)))
                                               (when (playing? this)
                                                 (start-playing this))))
                                          millis))
    (add-remove-classes-and-properties-for-animation this)))

(defn pause [this]
  (stop-playing this)
  (fire-event this (:id (:flow @(current this))))
  (let [image-element (dom/sel1 (keyword (str "#narrator-sections-center-overlay" @(id this))))]
    (dom/remove-class! image-element :narrator-sections-center-pause)
    (dom/remove-class! image-element :narrator-sections-center-play)
    (js/setTimeout #(dom/add-class! image-element :narrator-sections-center-pause)) 100))

(defn play [this]
  (start-playing this)
  (fire-event this (:id (:flow @(current this))))
  (let [image-element (dom/sel1 (keyword (str "#narrator-sections-center-overlay" @(id this))))]
    (dom/remove-class! image-element :narrator-sections-center-pause)
    (dom/remove-class! image-element :narrator-sections-center-play)
    (js/setTimeout #(dom/add-class! image-element :narrator-sections-center-play) 100)))

(defn clicked-flow [this flow]
  (let [keyframe_of_flow (get-narration-keyframe @(narration this) flow)]
    (set-keyframe this keyframe_of_flow)
    (when (playing? this) (pause this))))

(defn goto-first-subsection [this]
  "Set keyframe at first subsection flow of current flow"
  (when-let [first-subsection-flow (first (:flows (first (:subsections (:section @(current this))))))]
    (when-let [first-subsection-keyframe (get-narration-keyframe @(narration this) first-subsection-flow)]
      (set-keyframe this first-subsection-keyframe))))

(defn goto-previous-subsection [this]
  "Set keyframe at last subsection flow of previous subsection"
  (when-let [subsection-flow (last (:flows (:subsprev @(current this))))]
    (when-let [subsection-keyframe (get-narration-keyframe @(narration this) subsection-flow)]
      (set-keyframe this subsection-keyframe))))

(defn goto-next-subsection [this]
  "Set keyframe at first subsection flow of next subsection"
  (when (and (not (:subsnext @(current this)))
             (> (:numsubs @(current this)) 1))
    (goto-first-subsection this))
  (when-let [subsection-flow (first (:flows (:subsnext @(current this))))]
    (when-let [subsection-keyframe (get-narration-keyframe @(narration this) subsection-flow)]
      (set-keyframe this subsection-keyframe))))

;;
;; Rendered components
;;

(defn render-buttons [this]
  (let [number-sections (- (count @(narration this)) 1)]
    [:div.narrator-buttons {:id (str "narrator-buttons-" (instance-id this))}
     [:div {:style {:width "104px"}}
      [:img.narrator-button
       {:style {:max-height (if (= 0 @(keyframe this)) 0 64)}
        :src "data:image/svg+xml;base64,
   PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2ZXJzaW9uPSIxLjEiIGlkPSJDYXBhXzEiIHg9IjBweCIgeT0iMHB4IiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImVuYWJsZS1iYWNrZ3JvdW5kOm5ldyAwIDAgNTEyIDUxMjsiIHhtbDpzcGFjZT0icHJlc2VydmUiIHdpZHRoPSI1MTIiIGhlaWdodD0iNTEyIj48ZyB0cmFuc2Zvcm09Im1hdHJpeCgtMSAwIDAgMSA1MTIgMCkiPjxnPgoJPGc+CgkJPGc+CgkJCTxwYXRoIGQ9Ik0yMDYuNTkzLDE5Ni4zODRjLTEyLjQ4Ni0xNC41MzktMjUuNzQtMjguNTAzLTM5LjM3MS00MS45NzJjLTI3LjY3LTI3LjM0MS01Ny4xODgtNTIuNzQ5LTg3LjUtNzcuMTAyICAgICBDNjQuNzYsNjUuMjkxLDQ5Ljc4Miw1Mi42NjgsMzQuMDIsNDEuNzE4Yy03LjI3My01LjA1My0xNy4wNTktNy44OTYtMjQuODg4LTIuMzg1Yy04LjQ5OSw1Ljk4Mi02Ljk2NCwxNi41NjgtNi4xNTYsMjUuNTU2ICAgICBjMS45NzksMjIuMDQ1LDEuNzEyLDQ0LjQzLDIuMTEyLDY2LjU1NmMwLjI2NiwxNC42NCwwLjQ5OCwyOS4yNzksMC44MzYsNDMuOTE2Yy0xLjk1MSw1MC4wMjYtNC4xNDYsMTAwLjA0NC01LjE5OSwxNTAuMTA0ICAgICBjLTAuNDYzLDIyLjA0My0wLjc2Myw0NC4wNzktMC43MjEsNjYuMTI3YzAuMDIxLDExLjAyMSwwLjA5NiwyMi4wNDIsMC4yMzksMzMuMDYzYzAuMTI5LDkuOTIyLTAuMzQ2LDIwLjE0NCwwLjg2MywzMC4wMDQgICAgIGMxLjA1Nyw4LjYxMiw0Ljg2OCwxNi45NSwxMy42MywxOS45NzdjOS4zMDUsMy4yMTQsMTguNjM3LTEuNjU4LDI2LjM4My02LjU0OWMxNy4xOTUtMTAuODYsMzEuOTA4LTI1LjA3LDQ2LjYwNS0zOC45OTcgICAgIGMzMS42LTI5Ljk0Niw2Mi40NDMtNjAuNyw5My41LTkxLjIwN2MxNC44OTMtMTQuNjI4LDMxLjYyNi0yOC4wMzQsNDUuNDU2LTQzLjY2N2MxMi41MzItMTQuMTY3LDIwLjQ1OC0zMS41OTEsMTMuMzEyLTUwLjM4ICAgICBDMjMzLjIyLDIyNi4wMzMsMjE4Ljg0MywyMTAuNjQ3LDIwNi41OTMsMTk2LjM4NHogTTIxOS4xMjYsMjczLjM4MWMtMi4zMjYsMy4yODctNS4xMyw2LjM0OS03Ljg2Niw5LjI0MyAgICAgYy0zNS42ODYsMzcuNzIyLTcyLjIwNyw3NC42NTEtMTA5LjUzMSwxMTAuNzUzYy0xNS44OTEsMTUuMzctMzEuOTM0LDMwLjU5Ny00OC42NTUsNDUuMDU5ICAgICBjLTguNDc4LDcuMzMyLTE3LjM0LDE0LjU5MS0yNy44MjIsMTguNTU5Yy0xLjMyOCwwLjUwMi0yLjgxOSwwLjk0OS00LjEzNiwwLjQxNWMtMi4yMTctMC44OTktMi41NTUtMy44NDYtMi41NDUtNi4yMzkgICAgIGMwLjQ3Ni0xMTAuNzY1LDEuMTkzLTIyMS41MjgsMi4xNTMtMzMyLjI5YzAuMTc1LTIwLjE3NCwwLjkzNy00MC4zNDEsMS41NTgtNjAuNTA0YzAuMDE2LTAuNTE3LDAuMDgtMS4xMiwwLjUyMS0xLjM4OSAgICAgYzAuNDg5LTAuMjk4LDEuMTExLDAuMDA4LDEuNTk0LDAuMzE3YzIuNzUzLDEuNzY2LDUuMzM4LDMuNzc5LDcuOTE4LDUuNzg5YzY2LjkzNSw1Mi4xNDEsMTM4LjExNSwxMDMuNTcsMTg0Ljc0LDE3NS45NzEgICAgIGMzLjkwNCw2LjA2Myw3LjczNywxMi43MDYsNy41NDcsMTkuOTE1QzIyNC40NjEsMjY0LjI2MiwyMjIuMjE0LDI2OS4wMTksMjE5LjEyNiwyNzMuMzgxeiIgZGF0YS1vcmlnaW5hbD0iIzAwMDAwMCIgY2xhc3M9ImFjdGl2ZS1wYXRoIj48L3BhdGg+CgkJCTxwYXRoIGQ9Ik01MDkuMjk5LDI0My44MzZjLTYuNzcyLTE3LjgwNC0yMS4xNDktMzMuMTktMzMuMzk5LTQ3LjQ1M2MtMTIuNDg3LTE0LjUzOS0yNS43NC0yOC41MDMtMzkuMzcxLTQxLjk3MiAgICAgYy0yNy42Ny0yNy4zNDEtNTcuMTg4LTUyLjc0OS04Ny41LTc3LjEwMmMtMTQuOTYxLTEyLjAxOS0yOS45MzktMjQuNjQzLTQ1LjcwMi0zNS41OTJjLTcuMjc0LTUuMDUzLTE3LjA1OS03Ljg5Ni0yNC44ODktMi4zODUgICAgIGMtOC40OTgsNS45ODItNi45NjMsMTYuNTY4LTYuMTU1LDI1LjU1NmMxLjk3OSwyMi4wNDUsMS43MTIsNDQuNDMsMi4xMTIsNjYuNTU2YzAuMjY1LDE0LjY0LDAuNDk4LDI5LjI3OSwwLjgzNiw0My45MTYgICAgIGMtMS45NTEsNTAuMDI2LTQuMTQ2LDEwMC4wNDQtNS4xOTksMTUwLjEwNGMtMC40NjMsMjIuMDQzLTAuNzYzLDQ0LjA3OS0wLjcyMSw2Ni4xMjdjMC4wMjEsMTEuMDIxLDAuMDk2LDIyLjA0MiwwLjIzOSwzMy4wNjMgICAgIGMwLjEzLDkuOTIyLTAuMzQ2LDIwLjE0NCwwLjg2MywzMC4wMDRjMS4wNTcsOC42MTIsNC44NjgsMTYuOTUsMTMuNjI5LDE5Ljk3N2M5LjMwNiwzLjIxNCwxOC42MzgtMS42NTgsMjYuMzgzLTYuNTQ5ICAgICBjMTcuMTk2LTEwLjg2LDMxLjkwOS0yNS4wNyw0Ni42MDYtMzguOTk3YzMxLjYtMjkuOTQ2LDYyLjQ0My02MC43LDkzLjUtOTEuMjA3YzE0Ljg5My0xNC42MjgsMzEuNjI2LTI4LjAzNCw0NS40NTYtNDMuNjY3ICAgICBDNTA4LjUxOSwyODAuMDUsNTE2LjQ0NSwyNjIuNjI1LDUwOS4yOTksMjQzLjgzNnogTTQ5My40ODIsMjYyLjg2OGMtMC43MjQsMy44OTgtMi41OSw3LjM3My00Ljk0NywxMC41ODcgICAgIGMtMS45NjgsMi42ODUtNC4yNzgsNS4xODgtNi41NDcsNy42Yy00Ny4zNzMsNTAuMzc0LTk2LjQyNSw5OS4xNjktMTQ3LjA0NiwxNDYuMjc5Yy0xMS44MDcsMTAuOTg4LTIzLjkzMSwyMi4wNDQtMzguNDczLDI5LjAyMyAgICAgYy0yLjE3OSwxLjA0Ni00Ljk2NCwxLjkzNS02LjgzOSwwLjQxYy0xLjUzNS0xLjI1LTEuNjU3LTMuNTEtMS42NTMtNS40OWMwLjIzMS0xMTAuODY4LDAuOTcyLTIyMS43MzUsMi4yMjItMzMyLjU5NyAgICAgYzAuMTI1LTExLjA4NiwwLjI1NS0yMi4xNzIsMC4zOTEtMzMuMjU4YzAuMTAyLTguMzg2LTIuMDc4LTIwLjgwMiwxLjY4NC0yOC4zOTRjMS40MjgsMC4yMDYsNy42MDQsMi43MTEsNy42MDQsNC41NzQgICAgIGMyLjIxNCwxLjc5MSw0LjMyOCwzLjcyNiw2LjU5Miw1LjQ1NWM3LjA1OSw1LjM5MywxNC4xMDksMTAuNzk0LDIxLjEyOSwxNi4yMzdjNTcuODAyLDQ0LjgyNSwxMTcuMjA1LDkyLjAwNywxNTcuNjI0LDE1My45MzEgICAgIEM0OTAuMjU4LDI0NC45MzksNDk1LjE1OSwyNTMuODA5LDQ5My40ODIsMjYyLjg2OHoiIGRhdGEtb3JpZ2luYWw9IiMwMDAwMDAiIGNsYXNzPSJhY3RpdmUtcGF0aCI+PC9wYXRoPgoJCTwvZz4KCTwvZz4KPC9nPjwvZz4gPC9zdmc+"
        :on-click #(do
                     (when (playing? this) (pause this))
                     (set-keyframe this (max 0 (- @(keyframe this) 1))))}]]
     (if (playing? this)
       [:img.narrator-button
        {:src "data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDUxMiA1MTI7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iNjRweCIgaGVpZ2h0PSI2NHB4Ij4KPGc+Cgk8Zz4KCQk8Zz4KCQkJPHBhdGggZD0iTTE0Mi40MDcsMEgxMTUuMDFDODQuMjY1LDAsNTkuMjYyLDI1LjAwNCw1OS4yNjIsNTUuNzQ4djQwMC41MTRjMCwzMC43MzUsMjUuMDAzLDU1LjczOCw1NS43NDgsNTUuNzM4aDI3LjM5NyAgICAgYzMwLjczNSwwLDU1LjczOC0yNS4wMDQsNTUuNzQ4LTU1LjczOFY1NS43NDhDMTk4LjE1NCwyNS4wMDQsMTczLjE1MSwwLDE0Mi40MDcsMHogTTE2Mi45NDcsNDU2LjI2MiAgICAgYzAsMTEuMzE5LTkuMjEyLDIwLjU0MS0yMC41NDEsMjAuNTQxSDExNS4wMWMtMTEuMzI5LDAtMjAuNTUtOS4yMjEtMjAuNTQxLTIwLjU1VjU1LjczOGMwLTExLjMxOSw5LjIxMi0yMC41NDEsMjAuNTQxLTIwLjU0MSAgICAgdjAuMDFoMjcuMzk3YzExLjMxOSwwLDIwLjU0MSw5LjIxMiwyMC41NDEsMjAuNTQxVjQ1Ni4yNjJ6IiBmaWxsPSIjMDAwMDAwIi8+CgkJCTxwYXRoIGQ9Ik0zOTYuOTksMGgtMjcuMzc4Yy0zMC43NDQsMC01NS43NDgsMjUuMDA0LTU1Ljc0OCw1NS43Mzh2NDAwLjUxNGMwLDMwLjc0NCwyNS4wMDQsNTUuNzQ4LDU1Ljc0OCw1NS43NDhoMjcuMzc4ICAgICBjMzAuNzQ0LDAsNTUuNzQ4LTI1LjAwNCw1NS43NDgtNTUuNzM4VjU1Ljc0OEM0NTIuNzM4LDI1LjAwNCw0MjcuNzM1LDAsMzk2Ljk5LDB6IE00MTcuNTMxLDQ1Ni4yNTIgICAgIGMwLDExLjMxOS05LjIxMiwyMC41NDEtMjAuNTQxLDIwLjU0MWgtMjcuMzc4Yy0xMS4zMTksMC0yMC41NDEtOS4yMTItMjAuNTQxLTIwLjU0MVY1NS43MzhjMC0xMS4zMTksOS4yMTItMjAuNTQxLDIwLjU0MS0yMC41NDEgICAgIGgyNy4zNzhjMTEuMzE5LDAsMjAuNTQxLDkuMjEyLDIwLjU0MSwyMC41NDFWNDU2LjI1MnoiIGZpbGw9IiMwMDAwMDAiLz4KCQk8L2c+Cgk8L2c+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPC9zdmc+Cg=="
         :on-click #(pause this)}]
       [:img.narrator-button
        {:src "data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDUxMiA1MTI7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iNjRweCIgaGVpZ2h0PSI2NHB4Ij4KPGc+Cgk8Zz4KCQk8cGF0aCBkPSJNNDQ1Ljc1OSwxOTUuMjI4TDEzMC4xNzksMTMuMDQ1QzExNS4xNzksNC4zODcsMTAwLjQzNywwLDg2LjMzMywwYy0zNi43NTEsMC02MS40NCwyOS42NzYtNjEuNDQsNzMuODQ2djM2NC4zMTggICAgYzAsNDQuMTcsMjQuNjg4LDczLjgzNiw2MS40NCw3My44MzZjMTQuMDk0LDAsMjguODQ2LTQuMzg3LDQzLjg0Ni0xMy4wNDVsMzE1LjU4LTE4Mi4xODMgICAgYzI2LjY2Mi0xNS4zODEsNDEuMzQ3LTM2Ljk3MSw0MS4zNDctNjAuNzcyUzQ3Mi40MjEsMjEwLjYwOSw0NDUuNzU5LDE5NS4yMjh6IE00MjguMTQ2LDI4Ni4yOTVsLTMxNS41OCwxODIuMTczICAgIGMtOS40NDEsNS40NDUtMTguNTA5LDguMzM0LTI2LjI0Myw4LjMzNGMtMjMuNjY4LDAtMjYuMjI0LTI3LjAyNS0yNi4yMjQtMzguNjM5VjczLjg0NmMwLTExLjYxNSwyLjU1Ni0zOC42MzksMjYuMjMzLTM4LjYzOSAgICBjNy43MzQsMCwxNi44MTIsMi44OCwyNi4yNDMsOC4zMjVsMzE1LjU3MSwxODIuMTkyYzE1LjA4Niw4LjcwNiwyMy43MzUsMTkuNzM5LDIzLjczNSwzMC4yODYgICAgQzQ1MS44ODEsMjY2LjU1Niw0NDMuMjMyLDI3Ny41ODksNDI4LjE0NiwyODYuMjk1eiIgZmlsbD0iIzAwMDAwMCIvPgoJPC9nPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+Cjwvc3ZnPgo="
         :on-click #(play this)}])
     [:div {:style {:width "104px"}}
      [:img.narrator-button
       {:style {:max-height (if (= number-sections @(keyframe this)) 0 64)}
        :src "data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDUxMiA1MTI7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iNjRweCIgaGVpZ2h0PSI2NHB4Ij4KPGc+Cgk8Zz4KCQk8Zz4KCQkJPHBhdGggZD0iTTIwNi41OTMsMTk2LjM4NGMtMTIuNDg2LTE0LjUzOS0yNS43NC0yOC41MDMtMzkuMzcxLTQxLjk3MmMtMjcuNjctMjcuMzQxLTU3LjE4OC01Mi43NDktODcuNS03Ny4xMDIgICAgIEM2NC43Niw2NS4yOTEsNDkuNzgyLDUyLjY2OCwzNC4wMiw0MS43MThjLTcuMjczLTUuMDUzLTE3LjA1OS03Ljg5Ni0yNC44ODgtMi4zODVjLTguNDk5LDUuOTgyLTYuOTY0LDE2LjU2OC02LjE1NiwyNS41NTYgICAgIGMxLjk3OSwyMi4wNDUsMS43MTIsNDQuNDMsMi4xMTIsNjYuNTU2YzAuMjY2LDE0LjY0LDAuNDk4LDI5LjI3OSwwLjgzNiw0My45MTZjLTEuOTUxLDUwLjAyNi00LjE0NiwxMDAuMDQ0LTUuMTk5LDE1MC4xMDQgICAgIGMtMC40NjMsMjIuMDQzLTAuNzYzLDQ0LjA3OS0wLjcyMSw2Ni4xMjdjMC4wMjEsMTEuMDIxLDAuMDk2LDIyLjA0MiwwLjIzOSwzMy4wNjNjMC4xMjksOS45MjItMC4zNDYsMjAuMTQ0LDAuODYzLDMwLjAwNCAgICAgYzEuMDU3LDguNjEyLDQuODY4LDE2Ljk1LDEzLjYzLDE5Ljk3N2M5LjMwNSwzLjIxNCwxOC42MzctMS42NTgsMjYuMzgzLTYuNTQ5YzE3LjE5NS0xMC44NiwzMS45MDgtMjUuMDcsNDYuNjA1LTM4Ljk5NyAgICAgYzMxLjYtMjkuOTQ2LDYyLjQ0My02MC43LDkzLjUtOTEuMjA3YzE0Ljg5My0xNC42MjgsMzEuNjI2LTI4LjAzNCw0NS40NTYtNDMuNjY3YzEyLjUzMi0xNC4xNjcsMjAuNDU4LTMxLjU5MSwxMy4zMTItNTAuMzggICAgIEMyMzMuMjIsMjI2LjAzMywyMTguODQzLDIxMC42NDcsMjA2LjU5MywxOTYuMzg0eiBNMjE5LjEyNiwyNzMuMzgxYy0yLjMyNiwzLjI4Ny01LjEzLDYuMzQ5LTcuODY2LDkuMjQzICAgICBjLTM1LjY4NiwzNy43MjItNzIuMjA3LDc0LjY1MS0xMDkuNTMxLDExMC43NTNjLTE1Ljg5MSwxNS4zNy0zMS45MzQsMzAuNTk3LTQ4LjY1NSw0NS4wNTkgICAgIGMtOC40NzgsNy4zMzItMTcuMzQsMTQuNTkxLTI3LjgyMiwxOC41NTljLTEuMzI4LDAuNTAyLTIuODE5LDAuOTQ5LTQuMTM2LDAuNDE1Yy0yLjIxNy0wLjg5OS0yLjU1NS0zLjg0Ni0yLjU0NS02LjIzOSAgICAgYzAuNDc2LTExMC43NjUsMS4xOTMtMjIxLjUyOCwyLjE1My0zMzIuMjljMC4xNzUtMjAuMTc0LDAuOTM3LTQwLjM0MSwxLjU1OC02MC41MDRjMC4wMTYtMC41MTcsMC4wOC0xLjEyLDAuNTIxLTEuMzg5ICAgICBjMC40ODktMC4yOTgsMS4xMTEsMC4wMDgsMS41OTQsMC4zMTdjMi43NTMsMS43NjYsNS4zMzgsMy43NzksNy45MTgsNS43ODljNjYuOTM1LDUyLjE0MSwxMzguMTE1LDEwMy41NywxODQuNzQsMTc1Ljk3MSAgICAgYzMuOTA0LDYuMDYzLDcuNzM3LDEyLjcwNiw3LjU0NywxOS45MTVDMjI0LjQ2MSwyNjQuMjYyLDIyMi4yMTQsMjY5LjAxOSwyMTkuMTI2LDI3My4zODF6IiBmaWxsPSIjMDAwMDAwIi8+CgkJCTxwYXRoIGQ9Ik01MDkuMjk5LDI0My44MzZjLTYuNzcyLTE3LjgwNC0yMS4xNDktMzMuMTktMzMuMzk5LTQ3LjQ1M2MtMTIuNDg3LTE0LjUzOS0yNS43NC0yOC41MDMtMzkuMzcxLTQxLjk3MiAgICAgYy0yNy42Ny0yNy4zNDEtNTcuMTg4LTUyLjc0OS04Ny41LTc3LjEwMmMtMTQuOTYxLTEyLjAxOS0yOS45MzktMjQuNjQzLTQ1LjcwMi0zNS41OTJjLTcuMjc0LTUuMDUzLTE3LjA1OS03Ljg5Ni0yNC44ODktMi4zODUgICAgIGMtOC40OTgsNS45ODItNi45NjMsMTYuNTY4LTYuMTU1LDI1LjU1NmMxLjk3OSwyMi4wNDUsMS43MTIsNDQuNDMsMi4xMTIsNjYuNTU2YzAuMjY1LDE0LjY0LDAuNDk4LDI5LjI3OSwwLjgzNiw0My45MTYgICAgIGMtMS45NTEsNTAuMDI2LTQuMTQ2LDEwMC4wNDQtNS4xOTksMTUwLjEwNGMtMC40NjMsMjIuMDQzLTAuNzYzLDQ0LjA3OS0wLjcyMSw2Ni4xMjdjMC4wMjEsMTEuMDIxLDAuMDk2LDIyLjA0MiwwLjIzOSwzMy4wNjMgICAgIGMwLjEzLDkuOTIyLTAuMzQ2LDIwLjE0NCwwLjg2MywzMC4wMDRjMS4wNTcsOC42MTIsNC44NjgsMTYuOTUsMTMuNjI5LDE5Ljk3N2M5LjMwNiwzLjIxNCwxOC42MzgtMS42NTgsMjYuMzgzLTYuNTQ5ICAgICBjMTcuMTk2LTEwLjg2LDMxLjkwOS0yNS4wNyw0Ni42MDYtMzguOTk3YzMxLjYtMjkuOTQ2LDYyLjQ0My02MC43LDkzLjUtOTEuMjA3YzE0Ljg5My0xNC42MjgsMzEuNjI2LTI4LjAzNCw0NS40NTYtNDMuNjY3ICAgICBDNTA4LjUxOSwyODAuMDUsNTE2LjQ0NSwyNjIuNjI1LDUwOS4yOTksMjQzLjgzNnogTTQ5My40ODIsMjYyLjg2OGMtMC43MjQsMy44OTgtMi41OSw3LjM3My00Ljk0NywxMC41ODcgICAgIGMtMS45NjgsMi42ODUtNC4yNzgsNS4xODgtNi41NDcsNy42Yy00Ny4zNzMsNTAuMzc0LTk2LjQyNSw5OS4xNjktMTQ3LjA0NiwxNDYuMjc5Yy0xMS44MDcsMTAuOTg4LTIzLjkzMSwyMi4wNDQtMzguNDczLDI5LjAyMyAgICAgYy0yLjE3OSwxLjA0Ni00Ljk2NCwxLjkzNS02LjgzOSwwLjQxYy0xLjUzNS0xLjI1LTEuNjU3LTMuNTEtMS42NTMtNS40OWMwLjIzMS0xMTAuODY4LDAuOTcyLTIyMS43MzUsMi4yMjItMzMyLjU5NyAgICAgYzAuMTI1LTExLjA4NiwwLjI1NS0yMi4xNzIsMC4zOTEtMzMuMjU4YzAuMTAyLTguMzg2LTIuMDc4LTIwLjgwMiwxLjY4NC0yOC4zOTRjMS40MjgsMC4yMDYsNy42MDQsMi43MTEsNy42MDQsNC41NzQgICAgIGMyLjIxNCwxLjc5MSw0LjMyOCwzLjcyNiw2LjU5Miw1LjQ1NWM3LjA1OSw1LjM5MywxNC4xMDksMTAuNzk0LDIxLjEyOSwxNi4yMzdjNTcuODAyLDQ0LjgyNSwxMTcuMjA1LDkyLjAwNywxNTcuNjI0LDE1My45MzEgICAgIEM0OTAuMjU4LDI0NC45MzksNDk1LjE1OSwyNTMuODA5LDQ5My40ODIsMjYyLjg2OHoiIGZpbGw9IiMwMDAwMDAiLz4KCQk8L2c+Cgk8L2c+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPC9zdmc+Cg=="
        :on-click #(do
                     (when (playing? this) (pause this))
                     (set-keyframe this (min number-sections (+ @(keyframe this) 1))))}]]
     [:div {:style {:width "0px"}}
      [:img.narrator-button
       {:style {:max-height (if (playing? this) "0px" "64px")
                :margin-left (if (playing? this) "0px" "40px")}
        :src "data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDUxMiA1MTI7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iNjRweCIgaGVpZ2h0PSI2NHB4Ij4KPGc+Cgk8Zz4KCQk8cGF0aCBkPSJNNTAxLjg4MiwyMTAuNjg5Yy02LjQ1OS0xLjgzMy0xNC4xNjIsMC4xMjctMjAuNzI5LDAuNTg5Yy04LjIxOSwwLjU3OC0xNi40MzcsMS4xNTUtMjQuNjU2LDEuNzMzICAgIGMtMS41ODksMC4xMTItMy4xNzksMC4yMTUtNC43NjgsMC4zMjZjLTcuOTU2LTY4LjgwNC01My4yNjctMTMwLjg0Ni0xMTUuNzgyLTE2MC41MDJjLTMzLjA5OS0xNS43MDItNzAuMzEzLTIzLjQ5LTEwNi45Ni0yMi40MDUgICAgYy0zNi4wNTcsMS4wNjgtNzEuNDg0LDEyLjI3Ni0xMDIuODA0LDI5LjkyMkM2NC42MDYsOTUuMDQzLDExLjQzNiwxNTcuOTU0LDEuNTU2LDIyOS41MzUgICAgYy0yLjgzMiwyMC41MTQtMS41NjEsNDEuMDQ3LDEuOTA0LDYxLjQwN2MzLjQ4MSwyMC40NTMsOC41NjcsNDAuODQ4LDE1Ljg0NCw2MC4yOUMzMS44NDcsMzg0Ljc0Nyw1MS40NjgsNDE2LDc4LjQ3Niw0MzkuNzQ5ICAgIGMyOC4wNzUsMjQuNjg3LDYzLjc2MSwzOS45OSwxMDEuMjQ3LDQxLjc4MWM5LjI1OCwwLjQ0MiwxOC40MjMtMC4xODIsMjcuNTc4LTEuNTk2YzguMzA3LTEuMjgzLDE5LjM5Ni0yLjM1OCwyMi44MDktMTEuNDA2ICAgIGMxLjYwMy00LjI1LDEuNDEzLTkuMjcyLDEuNzA5LTEzLjc0NGMwLjM3LTUuNTkxLDAuNzI0LTExLjE4NCwxLjA2My0xNi43NzdjMC42NzgtMTEuMTk1LDEuNDIzLTIyLjM4MSwyLjE2Ny0zMy41NzEgICAgYzAuNzE5LTEwLjgyLDEuMzY3LTIxLjY0NSwxLjk4OS0zMi40N2MwLjIzMi00LjAzNC0zLjc1LTcuOTkxLTcuNzctNy43NzFjLTYzLjk5OSwzLjUxMS0xMDAuNzIxLTczLjMxMS04Ni41MTYtMTI4LjMwNiAgICBjMTMuODAyLTUzLjQzMSw3My40NC04Ny4wNzEsMTI2LjE1Mi03MC42NjJjMjcuMTI4LDguNDQ1LDUyLjU2MywyNy45NjQsNjQuMjc4LDU0LjM0N2MtMS4wNDUsMC4wODYtMi4wOTIsMC4xNTQtMy4xMzYsMC4yNDcgICAgYy0xMy45MjcsMS4yMzItMjguMDU5LDIuMjAzLTQxLjc5LDQuOTU5Yy0xMC45NzEsMi4yMDMtMjAuNDMyLDYuMzYtMjEuODksMTguNjA3Yy0yLjg0MiwyMy44NjUsMTQuODM0LDQzLjcwNiwyOC41NTgsNjEuMDM0ICAgIGMxNi41NjYsMjAuOTE2LDMzLjk1Myw0MS4yNTYsNTIuNjExLDYwLjMzOWM4Ljk1NSw5LjE1OSwxOC4wNDcsMTguMzkzLDI3LjkzMywyNi41NjFjOS4zMzcsNy43MTQsMTkuODk2LDEzLjQ1MSwzMi4zNzIsMTEuNTU4ICAgIGMyNC45OTYtMy43OTIsNDAuMDUxLTI4LjEwMSw1MS43NDktNDguMDI0YzEzLjE5MS0yMi40NjcsMjQuNjAyLTQ2LjExNCwzNC4zMzMtNzAuMjc3YzUuMzI4LTEzLjIzLDEwLjA0OS0yNi43MDUsMTQuMTM5LTQwLjM2OSAgICBDNTExLjI4NywyMzMuNDIsNTE3LjQsMjE1LjA5Miw1MDEuODgyLDIxMC42ODl6IE0yNzMuNTgzLDE0Ni43NzdjLTU4LjQzNC0xNi43OTYtMTE4LjU3NCwyMC4xOTktMTM5LjAyMSw3NS40NjYgICAgYy0xMS4wNDksMjkuODY3LTguNDk5LDY0LjIwOSw0LjAyMiw5My4xODhjMTQuMzExLDMzLjEyMSw0NS4yNDIsNjMuNjgyLDgyLjcyLDY0LjU1N2MtMC42NTQsMjYuNjY3LTIuMTEzLDUzLjI0OC0zLjcwNiw3OS44NzIgICAgYy0wLjA1NSwwLjkyOS0wLjEyNSwxLjkwMi0wLjYxOSwyLjY5MWMtMC44MjYsMS4zMTktMi41MzksMS43MDgtNC4wNzUsMS45NTVjLTQwLjc3NCw2LjU3OS04MS45NjQtNS4xNTQtMTE0Ljk2My0yOS41MDkgICAgYy0yNi4xMzEtMTkuMjg2LTQ2LjA3NC00Ni40MTYtNTkuNjQ5LTc1LjcwOGMtMTUuNzMxLTMzLjk0My0yNS41MjItNzMuODU3LTI0LjIwOC0xMTEuMzc0ICAgIGMyLjM2My02Ny40NDQsNDkuMzI4LTEyOC4wOTUsMTA0LjI0Ny0xNjMuNTczYzU1LjU5Mi0zNS45MTMsMTI2Ljc1LTUwLjA1OSwxOTAuNjgtMzAuMTY2ICAgIGMzMS4zMDEsOS43NCw2MC42MjksMjcuNzE0LDgyLjQ0NCw1Mi4zNTJjMjEuMTI2LDIzLjg2MSwzNS4zMjQsNTMuNDk3LDQwLjk2Myw4NC44MjhjMS4zNjYsNy41OSwyLjAwNCwxNS4yMjYsMi4xOTcsMjIuOTMyICAgIGMwLjAwMSwwLjA3NSwwLjAxMywwLjE0NCwwLjAxNiwwLjIxOGMtNi4yNTMsMC40MjYtMTIuNTA1LDAuODYzLTE4Ljc1OSwxLjI4M2MtMC4wNjQsMC4wMDMtMC4xMjksMC4wMDYtMC4xOTMsMC4wMDkgICAgYy0yMi44NjksMS4wOC00NS43NTQsMS43NzItNjguNjI1LDIuODUyYy0wLjAzLTAuMS0wLjA2MS0wLjE5OS0wLjA5Mi0wLjI5OEMzMzYuMjc0LDE4NC40MDQsMzA3Ljc5MSwxNTYuNjA5LDI3My41ODMsMTQ2Ljc3N3ogICAgIE00OTYuMzg3LDIzMC4xMjZjLTAuNzk2LDMuMDQ5LTEuNjI3LDYuMDkxLTIuNDksOS4xMjRjLTguODg2LDMxLjIzNC0yMS4xOTMsNjEuNDk0LTM2LjYzNyw5MC4wNiAgICBjLTguODQ2LDE2LjM2My0xOC44MTYsMzIuMjk5LTMxLjcwNSw0NS43MWMtNS4zMzIsNS41NDgtMTEuNjI4LDEwLjg3Mi0xOS4yNTMsMTEuOTA2Yy05Ljk2OCwxLjM1Mi0xOS4yNC00Ljg5MS0yNy4xNjUtMTEuMDg1ICAgIGMtMzMuMzY5LTI2LjA4My02My4wMTItNTYuOTU2LTg3LjQ4OS05MS41MjFjLTMuOTg2LTUuNjMtNy45LTExLjQ5MS05LjcyOC0xOC4xNDNjLTEuODI4LTYuNjUyLTEuMjYyLTE0LjMxOSwzLjA3LTE5LjY4OCAgICBjNS4xMjYtNi4zNTMsMTMuOTY0LTguMDM0LDIyLjA0Mi05LjIxMmM1NC44MDMtNy45OTIsMTA5LjkyNC0xMi4wODcsMTY1LjI3My0xMy4xMTJjNC43NTItMC4xMjUsMjMuMjU0LTUuMjczLDI0LjQ4NiwyLjUyOSAgICBDNDk2Ljk3MiwyMjcuODQsNDk2LjY4MSwyMjkuMDAzLDQ5Ni4zODcsMjMwLjEyNnoiIGZpbGw9IiMwMDAwMDAiLz4KCTwvZz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8L3N2Zz4K"
        :on-click #(set-keyframe this @(keyframe this))}]]]))


(defn render-sections [this sections]
  [:div.narrator-sections
    (doall
      (for [section sections]
        [:div.narrator-section {:key (gensym "n-sct-")
                                :class [(when (:subsections section) "has-narrator-subsections")]}
         (doall
           (for [flow (:flows section)]
             [:span.narrator-flow {:key                     (gensym "n-sct-fl-")
                                   :id                      (get-element-id flow)
                                   :dangerouslySetInnerHTML #js{:__html (:html flow)}
                                   :on-click                #(do (goto-first-subsection this)
                                                                 (clicked-flow this flow))}]))
         [:div.narrator-subsection-frame
          [:img.narrator-subsection-frame-left
           {:src      "data:image/svg+xml;base64,
PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2ZXJzaW9uPSIxLjEiIGlkPSJDYXBhXzEiIHg9IjBweCIgeT0iMHB4IiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImVuYWJsZS1iYWNrZ3JvdW5kOm5ldyAwIDAgNTEyIDUxMjsiIHhtbDpzcGFjZT0icHJlc2VydmUiIHdpZHRoPSI1MTIiIGhlaWdodD0iNTEyIj48ZyB0cmFuc2Zvcm09Im1hdHJpeCgtMSAwIDAgMSA1MTIgMCkiPjxnPgoJPGc+CgkJPHBhdGggZD0iTTQxNy44MzgsMjQxLjk5OGMtNC40NTgtOS42NDEtMTAuODYyLTE4LjYtMTYuNjUxLTI3LjQ3OGMtMjMuNjQ2LTM2LjI2Ny01MS4yMy02OS43MDYtNzcuNjEzLTEwMy45NjcgICAgYy0xMy4wNzUtMTYuOTgtMjUuODYyLTM0LjIwMi0zNy42OTEtNTIuMDhjLTUuOTA2LTguOTI1LTExLjU3LTE4LjAxMS0xNi45MDItMjcuMjkxYy00LjU5Ni04LjAwMi04LjY5NC0xNi42NjQtMTUuNDcxLTIzLjEyNCAgICBjLTYuNzY4LTYuNDUyLTE1LjY2NC05Ljc0Ni0yNC45MjItNy4xODljLTExLjAyLDMuMDQ0LTIxLjYxNSw5Ljg1NC0zMS45MDMsMTQuNzUzYy0yMi4xMiwxMC41MzMtNDQuMzY1LDIwLjgyOC02Ni41ODksMzEuMTQxICAgIGMtMTEuMTEsNS4xNTUtMjIuNDQ1LDkuODkyLTMzLjMzLDE1LjUwOWMtMy4zOTEsMS43NDktNC41NjksNi42OTEtMi41ODcsOS44NjRjMTEuNDQ0LDE4LjMyMywyMi44NDEsMzYuNzE2LDM0LjgzNSw1NC42ODggICAgYzYuOTg0LDEwLjQ2NSwxNC4zNzksMjAuNjEyLDIyLjAyNiwzMC41NzVjNS44MTksOC41NzUsMTEuNzQ1LDE3LjA3OCwxNy43NDgsMjUuNTI1YzYuNzc0LDkuNTMyLDEzLjY0LDE4Ljk5OSwyMC41NDQsMjguNDM3ICAgIGM2LjQ1MSw4LjgyLDEzLjg4MSwxNy40NDIsMTkuMjIzLDI2Ljk5NGM1LjcxNSwxMC4yMTUsMC4wMTYsMTcuMzcyLTYuMjc1LDI1LjY1M2MtNy40NTUsOS44MTYtMTQuOTIsMTkuNjI1LTIyLjM4LDI5LjQzNyAgICBjLTE0LjkyMSwxOS42MjUtMjkuODMzLDM5LjI1Ny00NC43NjIsNTguODc1Yy03LjQ2Niw5LjgxMi0xNC45NjIsMTkuNjAxLTIyLjQ0MywyOS40MDFjLTMuNjI4LDQuNzUxLTcuMjU1LDkuNTAzLTEwLjg4MiwxNC4yNTQgICAgYy0zLjQzMiw0LjQ5NC03LjgxNiw4Ljk5NC05Ljk5OCwxNC4yNjZjLTQuNjYsMTEuMjYzLDQuNjk2LDE3LjU4OCwxMi40OTQsMjMuNTQxYzkuOTM1LDcuNTgzLDE5LjUxMiwxNS42OTMsMjkuMjUxLDIzLjUyNiAgICBjMTguOTIxLDE1LjIxNywzNy43NzIsMzAuNTEyLDU2LjU3Niw0NS44NzRjOC4zOTksNi44NjMsMTYuNTQsMTIuMjQ4LDI3LjIxNCw2LjE3OGM5LjY3LTUuNDk5LDE4LjM0LTE0LjEyNSwyNi4yNDYtMjEuODQ2ICAgIGMxNi41LTE2LjExNiwzMS44NzItMzMuNTAzLDQ2LjkyOC01MC45NmMzMC4zNDQtMzUuMTg3LDU4Ljk1My03MS45MjMsODcuMDg3LTEwOC44ODdjNy4wNTgtOS4yNzIsMTQuMDc5LTE4LjU3MiwyMS4wODktMjcuODgxICAgIGM2LjUyLTguNjU5LDE0LjMwNi0xNy4yMzksMTkuMDQ1LTI3LjA1NEM0MjIuNjkyLDI2Mi40OCw0MjIuNTk1LDI1Mi4yODgsNDE3LjgzOCwyNDEuOTk4eiBNMzk3LjMxLDI3NS4wMTIgICAgYy02LjI2NCw4LjgwNy0xMi43NSwxNy40NjQtMTkuMTQ2LDI2LjE3NWMtNDAuMzM1LDU0LjkzOS04MS43NzgsMTA4Ljg2OS0xMjguNDg1LDE1OC42MDQgICAgYy0xMS40NjksMTIuMjEyLTIzLjI1MSwyNC4yNTYtMzYuNzUyLDM0LjE3NmMtMS40OTQsMS4wOTgtMy4xOCwyLjIxOS01LjAyNywyLjA3NmMtMS41NTctMC4xMi0yLjg4Ny0xLjExNy00LjExOS0yLjA3NSAgICBjLTMwLjQ2OS0yMy42NjYtNjEuOTY5LTQ2LjQ0MS05MS41MTYtNzEuMjQ2Yy0xLjczNC0xLjQ1Ni00LjE1Ni0zLjE4Mi01LjU1NS01LjAwN2MtMS4zNzUtMS43OTYtMS4xMDYtMS45ODEsMC4yNzEtMy43OTggICAgYzM1LjI4OC00Ni41MjYsNzEuMzI2LTkyLjYzMiwxMDguMjA4LTEzNy43NGM4LjUzNi0xMC40NCwxNS4zOTItMjMuOTk0LDEyLjAzMS0zNy4wNTRjLTIuMDA3LTcuOC03LjM2Ny0xNC4yMjktMTIuNTI0LTIwLjQxNSAgICBjLTE5LjU1Ny0yMy40NjEtMzguMzYxLTQ3LjU0Ny01Ni4zNzctNzIuMjFjLTMuOTMyLTUuMzgyLTQ5Ljc2NS03My43MDQtNDcuOTMxLTc0LjUxM2MxOC42MDMtOC4xODMsMzYuODE5LTE3LjQxNyw1NS4yMzEtMjUuOTg0ICAgIGMyMS4wMTgtOS43OCw0MS44OS0yMC42MTUsNjMuNDQ1LTI5LjE1OGM4LjQzNi0zLjM0MywxMy4zMDMsMS4yNzQsMTcuODg4LDguMTVjNS4xOTIsNy43ODcsOS4zNTEsMTYuMjg4LDE0LjIxNiwyNC4yODcgICAgYzEwLjYyMiwxNy40NjYsMjIuMzMyLDM0LjI0NSwzNC41MTQsNTAuNjU0YzI0LjQ0MiwzMi45MjcsNTAuODI4LDY0LjM3NSw3NC44MTMsOTcuNjQ1YzUuODg4LDguMTY3LDExLjYzLDE2LjQ0LDE3LjE0NywyNC44NiAgICBjNS41OCw4LjUxMywxMS44ODEsMTcuNDExLDE1LjkzNiwyNi43NzhDNDA3LjgxLDI1OC45OTEsNDAyLjk3OSwyNjcuMDM4LDM5Ny4zMSwyNzUuMDEyeiIgZGF0YS1vcmlnaW5hbD0iIzAwMDAwMCIgY2xhc3M9ImFjdGl2ZS1wYXRoIj48L3BhdGg+Cgk8L2c+CjwvZz48L2c+IDwvc3ZnPg=="
            :on-click #(goto-previous-subsection this)}]
          [:img.narrator-subsection-frame-right
           {:src      "data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDUxMiA1MTI7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iNjRweCIgaGVpZ2h0PSI2NHB4Ij4KPGc+Cgk8Zz4KCQk8cGF0aCBkPSJNNDE3LjgzOCwyNDEuOTk4Yy00LjQ1OC05LjY0MS0xMC44NjItMTguNi0xNi42NTEtMjcuNDc4Yy0yMy42NDYtMzYuMjY3LTUxLjIzLTY5LjcwNi03Ny42MTMtMTAzLjk2NyAgICBjLTEzLjA3NS0xNi45OC0yNS44NjItMzQuMjAyLTM3LjY5MS01Mi4wOGMtNS45MDYtOC45MjUtMTEuNTctMTguMDExLTE2LjkwMi0yNy4yOTFjLTQuNTk2LTguMDAyLTguNjk0LTE2LjY2NC0xNS40NzEtMjMuMTI0ICAgIGMtNi43NjgtNi40NTItMTUuNjY0LTkuNzQ2LTI0LjkyMi03LjE4OWMtMTEuMDIsMy4wNDQtMjEuNjE1LDkuODU0LTMxLjkwMywxNC43NTNjLTIyLjEyLDEwLjUzMy00NC4zNjUsMjAuODI4LTY2LjU4OSwzMS4xNDEgICAgYy0xMS4xMSw1LjE1NS0yMi40NDUsOS44OTItMzMuMzMsMTUuNTA5Yy0zLjM5MSwxLjc0OS00LjU2OSw2LjY5MS0yLjU4Nyw5Ljg2NGMxMS40NDQsMTguMzIzLDIyLjg0MSwzNi43MTYsMzQuODM1LDU0LjY4OCAgICBjNi45ODQsMTAuNDY1LDE0LjM3OSwyMC42MTIsMjIuMDI2LDMwLjU3NWM1LjgxOSw4LjU3NSwxMS43NDUsMTcuMDc4LDE3Ljc0OCwyNS41MjVjNi43NzQsOS41MzIsMTMuNjQsMTguOTk5LDIwLjU0NCwyOC40MzcgICAgYzYuNDUxLDguODIsMTMuODgxLDE3LjQ0MiwxOS4yMjMsMjYuOTk0YzUuNzE1LDEwLjIxNSwwLjAxNiwxNy4zNzItNi4yNzUsMjUuNjUzYy03LjQ1NSw5LjgxNi0xNC45MiwxOS42MjUtMjIuMzgsMjkuNDM3ICAgIGMtMTQuOTIxLDE5LjYyNS0yOS44MzMsMzkuMjU3LTQ0Ljc2Miw1OC44NzVjLTcuNDY2LDkuODEyLTE0Ljk2MiwxOS42MDEtMjIuNDQzLDI5LjQwMWMtMy42MjgsNC43NTEtNy4yNTUsOS41MDMtMTAuODgyLDE0LjI1NCAgICBjLTMuNDMyLDQuNDk0LTcuODE2LDguOTk0LTkuOTk4LDE0LjI2NmMtNC42NiwxMS4yNjMsNC42OTYsMTcuNTg4LDEyLjQ5NCwyMy41NDFjOS45MzUsNy41ODMsMTkuNTEyLDE1LjY5MywyOS4yNTEsMjMuNTI2ICAgIGMxOC45MjEsMTUuMjE3LDM3Ljc3MiwzMC41MTIsNTYuNTc2LDQ1Ljg3NGM4LjM5OSw2Ljg2MywxNi41NCwxMi4yNDgsMjcuMjE0LDYuMTc4YzkuNjctNS40OTksMTguMzQtMTQuMTI1LDI2LjI0Ni0yMS44NDYgICAgYzE2LjUtMTYuMTE2LDMxLjg3Mi0zMy41MDMsNDYuOTI4LTUwLjk2YzMwLjM0NC0zNS4xODcsNTguOTUzLTcxLjkyMyw4Ny4wODctMTA4Ljg4N2M3LjA1OC05LjI3MiwxNC4wNzktMTguNTcyLDIxLjA4OS0yNy44ODEgICAgYzYuNTItOC42NTksMTQuMzA2LTE3LjIzOSwxOS4wNDUtMjcuMDU0QzQyMi42OTIsMjYyLjQ4LDQyMi41OTUsMjUyLjI4OCw0MTcuODM4LDI0MS45OTh6IE0zOTcuMzEsMjc1LjAxMiAgICBjLTYuMjY0LDguODA3LTEyLjc1LDE3LjQ2NC0xOS4xNDYsMjYuMTc1Yy00MC4zMzUsNTQuOTM5LTgxLjc3OCwxMDguODY5LTEyOC40ODUsMTU4LjYwNCAgICBjLTExLjQ2OSwxMi4yMTItMjMuMjUxLDI0LjI1Ni0zNi43NTIsMzQuMTc2Yy0xLjQ5NCwxLjA5OC0zLjE4LDIuMjE5LTUuMDI3LDIuMDc2Yy0xLjU1Ny0wLjEyLTIuODg3LTEuMTE3LTQuMTE5LTIuMDc1ICAgIGMtMzAuNDY5LTIzLjY2Ni02MS45NjktNDYuNDQxLTkxLjUxNi03MS4yNDZjLTEuNzM0LTEuNDU2LTQuMTU2LTMuMTgyLTUuNTU1LTUuMDA3Yy0xLjM3NS0xLjc5Ni0xLjEwNi0xLjk4MSwwLjI3MS0zLjc5OCAgICBjMzUuMjg4LTQ2LjUyNiw3MS4zMjYtOTIuNjMyLDEwOC4yMDgtMTM3Ljc0YzguNTM2LTEwLjQ0LDE1LjM5Mi0yMy45OTQsMTIuMDMxLTM3LjA1NGMtMi4wMDctNy44LTcuMzY3LTE0LjIyOS0xMi41MjQtMjAuNDE1ICAgIGMtMTkuNTU3LTIzLjQ2MS0zOC4zNjEtNDcuNTQ3LTU2LjM3Ny03Mi4yMWMtMy45MzItNS4zODItNDkuNzY1LTczLjcwNC00Ny45MzEtNzQuNTEzYzE4LjYwMy04LjE4MywzNi44MTktMTcuNDE3LDU1LjIzMS0yNS45ODQgICAgYzIxLjAxOC05Ljc4LDQxLjg5LTIwLjYxNSw2My40NDUtMjkuMTU4YzguNDM2LTMuMzQzLDEzLjMwMywxLjI3NCwxNy44ODgsOC4xNWM1LjE5Miw3Ljc4Nyw5LjM1MSwxNi4yODgsMTQuMjE2LDI0LjI4NyAgICBjMTAuNjIyLDE3LjQ2NiwyMi4zMzIsMzQuMjQ1LDM0LjUxNCw1MC42NTRjMjQuNDQyLDMyLjkyNyw1MC44MjgsNjQuMzc1LDc0LjgxMyw5Ny42NDVjNS44ODgsOC4xNjcsMTEuNjMsMTYuNDQsMTcuMTQ3LDI0Ljg2ICAgIGM1LjU4LDguNTEzLDExLjg4MSwxNy40MTEsMTUuOTM2LDI2Ljc3OEM0MDcuODEsMjU4Ljk5MSw0MDIuOTc5LDI2Ny4wMzgsMzk3LjMxLDI3NS4wMTJ6IiBmaWxsPSIjMDAwMDAwIi8+Cgk8L2c+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPGc+CjwvZz4KPC9zdmc+Cg=="
            :on-click #(goto-next-subsection this)}]
          [:div.narrator-subsection-frame-hider
           [:div.narrator-susbection-carousel
            {:style {:width (str (* 100 (count (:subsections section))) "%")}}
            (doall
              (for [subsection (:subsections section)]
                [:div.narrator-subsection {:key (gensym "n-ssct-")}
                 (doall
                   (for [flow (:flows subsection)]
                     [:span.narrator-flow {:key                     (gensym "n-sct-fl-")
                                           :id                      (get-element-id flow)
                                           :dangerouslySetInnerHTML #js{:__html (:html flow)}
                                           :on-click                #(clicked-flow this flow)}]))]))]]]]))
    [:div.narrator-sections-center
     [(keyword (str "div#narrator-sections-center-overlay" @(id this)))]]])


(defn timeline-render [this sections]
  [:div.narrator-frame
   [render-sections this sections]
   [render-buttons this]])

(defn toggle-play-state [this]
  (if (playing? this) (pause this) (play this)))

(defn render [this narrator-attrs]
  (when (not (has-attrs this))
    (ctor-attrs this))
  (let [sections @(get narrator-attrs "sections")
        paused  @(get narrator-attrs "paused")
        font-min @(get narrator-attrs "font-size-min--section")
        font-max @(get narrator-attrs "font-size-max--section")
        triggered @(get narrator-attrs "trigger")] ;; whenever triggered, restart play from beginning after 1 second
    (reset! (narration this) (get-narration sections))
    (if triggered
      (do
        (reset! (get narrator-attrs "trigger") "trigger") false
        (js/setTimeout #(do
                          (pause this)
                          (set-keyframe this 0)
                          (when (not paused) (play this)) 1000)))
      (if paused (when (playing? this) (pause this)) (when (not (playing? this)) (play this))))
    [:div {:style {:height "100%" :width "100%"}}
     [:style (css/get-styles font-min font-max @(id this))]
     (timeline-render this sections)]))
