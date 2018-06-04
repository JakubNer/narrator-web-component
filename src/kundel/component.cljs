(ns kundel.component
  (:require
    [reagent.core :as r]
    [debux.cs.core :refer-macros [clog dbg break]]))

(defn fire-event [this event-text]
  "Dispatch 'timeline' events."
  (let [event (.createEvent js/document "Event")]
    (.initEvent event "timeline" true true)
    (aset event "detail" event-text)
    (.dispatchEvent this event)))

(def color (r/atom "red"))

(defn render [this attrs]
  (let [sections @(get attrs "sections")
        font-size-min--section @(get attrs "font-size-min--section")
        font-size-max--section @(get attrs "font-size-max--section")]
    (when @(get attrs "trigger") ;; whenever triggered, make divs not visible, and animate visible after 500ms
      (reset! (get attrs "trigger") false)
      (js/setTimeout #(reset! color "red") 2000)
      (js/setTimeout #(reset! color "green") 4000)
      (js/setTimeout #(reset! color "blue") 6000)
      (js/setTimeout #(reset! color "grey") 8000))
    [:div {:style {:height "100%"
                   :width "100%"
                   :background-color @color
                   :display "flex"
                   :flex-direction "column"
                   :justify-content "space-evenly"}}]))

