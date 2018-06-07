(defproject clojurescript-w3c-custom-element  "0.0.1"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [reagent "0.8.0"]
                 [philoskim/debux "0.4.1"]
                 [prismatic/dommy "1.1.0"]]

  :plugins [[lein-cljsbuild "1.1.7"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js" "target"]

  :figwheel {:nrepl-port 7002}

  :profiles
  {:dev
   {:repl-options {:nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]}
    :dependencies [[binaryage/devtools "0.9.9"]
                   [com.cemerick/piggieback "0.2.2"]
                   [figwheel-sidecar "0.5.15"]
                   [org.clojure/tools.nrepl "0.2.13"]]
    :plugins      [[lein-figwheel "0.5.15"]]}}

  :cljsbuild
  {:builds
   {:dev {:source-paths ["src" "test"]
          :figwheel true
          :compiler {:main kundel.narrator
                     :output-to "resources/public/js/narrator.js"
                     :output-dir "resources/public/js/out/"
                     :asset-path "js/out"
                     :optimizations :none
                     :source-map true
                     :source-map-timestamp true
                     :preloads [devtools.preload]
                     :external-config {:devtools/config {:features-to-install :all}}}}
    :min {:source-paths ["src"]
          :compiler {:output-to "resources/public/js/narrator.js"
                     :output-dir "resources/public/js/min-out/"
                     :asset-path "js/min-out"
                     :optimizations   :advanced
                     :closure-defines {goog.DEBUG false}
                     :pseudo-names    false ;; true for debugging cryptic errors, comment out otherwise
                     :pretty-print    false}}}}) ;; true for debugging cryptic errors, false otherwise
