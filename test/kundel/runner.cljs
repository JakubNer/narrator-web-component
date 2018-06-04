(ns kundel.runner
  (:require [cljs.test :refer-macros [run-tests]]
            [kundel.smoke-test]))

(defn ^:export run []
  (run-tests 'kundel.smoke-test))
