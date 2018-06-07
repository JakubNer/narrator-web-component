(ns kundel.component
  (:require
    [reagent.core :as r]
    [debux.cs.core :refer-macros [clog dbg break]]
    [dommy.core :as dom]
    [kundel.css :as css]))

(def this (r/atom nil))
(def previous-paused-attribute (r/atom nil))

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
     (cons {:section (first sects) :subsection nil :flow (first flows) :subsnext nil :subsprev nil}
           (get-narration sects (rest flows) subs subflows subsprev))
     (if (seq subflows)
       (cons {:section (first sects) :subsection (first subs) :flow (first subflows) :subsnext (second subs) :subsprev subsprev}
             (get-narration sects nil subs (rest subflows) subsprev))
       (if (seq subs)
         (get-narration sects nil (rest subs) (:flows (second subs)) (first subs))
         (when (seq sects)
           (get-narration (rest sects))))))))

(defn get-narration-keyframe [narration flow]
  "retrieves narration keyframe for flow."
  (first (keep-indexed #(when (= flow (:flow %2)) %1) narration)))

(def narration (r/atom nil))
(def keyframe (r/atom nil))
(def current (r/atom nil))
(def timeout (r/atom nil))

(defn get-element-id [flow]
  "Retrieve HTML ID for 'flow' element"
  (str "narrator-flow-" (:id flow)))

(defn find-subsection-js-for-flow-js [flow-js-element]
  "Find a JS element that's a parent of 'flow-js-element' with .narrator-subsection "
  (when flow-js-element
    (when-let [parent (dom/parent flow-js-element)]
      (if (dom/has-class? parent "narrator-subsection")
        parent
        (find-subsection-js-for-flow-js parent)))))

(defn find-section-js-for-flow-js [flow-js-element]
  "Find a JS element that's a parent of 'flow-js-element' with .narrator-section "
  (when flow-js-element
    (when-let [parent (dom/parent flow-js-element)]
      (if (dom/has-class? parent "narrator-section")
        parent
        (find-subsection-js-for-flow-js parent)))))

(defn add-remove-classes-for-animation []
  "go through all IDs and animate as required"
  (doseq [rec @narration
          :let [flow (:flow rec)
                flow-js-element (dom/sel1 (str "#"(get-element-id flow)))
                subsection (:subsection rec)
                section (:section rec)]
          :when flow-js-element
          :when (not= rec @current)]
    (dom/remove-class! flow-js-element "narrator-current")
    (when subsection
      (when-let [subsection-js-element (find-subsection-js-for-flow-js flow-js-element)]
        (dom/remove-class! subsection-js-element "narrator-current")))
    (when section
      (when-let [section-js-element (find-section-js-for-flow-js flow-js-element)]
        (dom/remove-class! section-js-element "narrator-current"))))
  (let [current-flow (:flow @current)
        current-flow-js-element (dom/sel1 (str "#" (get-element-id current-flow)))]
    (when current-flow-js-element
      (dom/add-class! current-flow-js-element "narrator-current")
      (when-let [subsection-js-element (find-subsection-js-for-flow-js current-flow-js-element)]
        (dom/add-class! subsection-js-element "narrator-current"))
      (when-let [section-js-element (find-section-js-for-flow-js current-flow-js-element)]
        (dom/add-class! section-js-element "narrator-current")))))

(defn playing? []
  (not (nil? @timeout)))

(defn fire-event [id]
  "Dispatch 'timeline' events."
  (let [event (.createEvent js/document "Event")]
    (.initEvent event "timeline" true true)
    (aset event "id" id)
    (aset event "playing" (playing?))
    (.dispatchEvent @this event)))

(defn stop-playing []
  (when (playing?)
    (js/clearTimeout @timeout)
    (reset! timeout nil)))

(defn set-keyframe [new-keyframe]
  (cond
    (< new-keyframe 0)
    (do
      (stop-playing)
      (reset! keyframe 0)
      (reset! current (first @narration))
      (fire-event (:id (:flow @current))))
    (>= new-keyframe (count @narration))
    (do
      (stop-playing)
      (reset! keyframe (- (count @narration) 1))
      (reset! current (last @narration))
      (fire-event (:id (:flow @current))))
    :else
    (do
      (reset! keyframe new-keyframe)
      (reset! current (nth @narration new-keyframe))
      (fire-event (:id (:flow @current)))))
  (add-remove-classes-for-animation))

(defn start-playing []
  (stop-playing)
  (let [seconds (:seconds (:flow (nth @narration @keyframe)))
        millis (* 1000 seconds)]
    (reset! timeout (js/setTimeout #(do
                                      (when (playing?)
                                        (set-keyframe (+ 1 @keyframe))
                                        (when (playing?)
                                          (start-playing))))
                                   millis))))

(defn pause []
  (stop-playing)
  (fire-event (:id (:flow @current)))
  (let [image-element (dom/sel1 :#narrator-sections-center-overlay)]
    (dom/remove-class! image-element :narrator-sections-center-play)
    (dom/add-class! image-element :narrator-sections-center-pause)
    (dom/set-style! image-element :opacity ".5")
    (js/setTimeout #(dom/set-style! image-element :opacity "0") 500)))

(defn play []
  (start-playing)
  (fire-event (:id (:flow @current)))
  (let [image-element (dom/sel1 :#narrator-sections-center-overlay)]
    (dom/remove-class! image-element :narrator-sections-center-pause)
    (dom/add-class! image-element :narrator-sections-center-play)
    (dom/set-style! image-element :opacity ".5")
    (js/setTimeout #(dom/set-style! image-element :opacity "0") 500)))

(defn clicked-flow [flow]
  (let [keyframe_of_flow (get-narration-keyframe @narration flow)]
    (if (not= keyframe_of_flow @keyframe)
      (pause)
      (if (playing?)
        (pause)
        (play)))
    (set-keyframe keyframe_of_flow)))
;;
;; Rendered components
;;

(defn timeline-render [sections]
  [:div.narrator-sections
   [:div.narrator-sections-center
    [:div#narrator-sections-center-overlay]]
   (doall
     (for [section sections]
       [:div.narrator-section {:key   (gensym "n-sct-")
                               :style {:width "95%"}}
        (doall
          (for [flow (:flows section)]
            [:span.narrator-flow {:key (gensym "n-sct-fl-")
                                  :id (get-element-id flow)
                                  :dangerouslySetInnerHTML #js{:__html (:html flow)}
                                  :on-click #(clicked-flow flow)}]))]))])

(defn render [_this attrs]
  (let [sections @(get attrs "sections")
        paused  @(get attrs "paused")
        previous-paused @previous-paused-attribute
        font-min @(get attrs "font-size-min--section")
        font-max @(get attrs "font-size-max--section")
        triggered @(get attrs "trigger")] ;; whenever triggered, make divs not visible, and animate visible after 500ms]
    (reset! this _this)
    (reset! narration (get-narration sections))
    (if triggered
      (do
        (reset! (get attrs "trigger") false)
        (js/setTimeout #(do
                          (set-keyframe 0)
                          (when (not paused) (play)) 1000)))
      (when (not= paused previous-paused)
        (if paused (when (playing?) (pause)) (when (not (playing?)) (play)))
        (reset! previous-paused-attribute paused)))
    [:div
     [:style (css/get-styles font-min font-max)]
     (timeline-render sections)]))
