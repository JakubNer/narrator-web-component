(ns kundel.css)

(defn get-styles [font-min font-max]
  (str "

  .narrator-section {
    -webkit-transition: font-size .5s ease-in-out;
    -moz-transition: font-size .5s ease-in-out;
    -o-transition: font-size .5s ease-in-out;
    transition: font-size .5s ease-in-out;
  }

  .narrator-section.narrator-current {
    font-size: " font-max ";
  }

  .narrator-section:not(.narrator-current) {
    font-size: " font-min ";
  }

  "))
