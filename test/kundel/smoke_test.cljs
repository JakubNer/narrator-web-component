(ns kundel.smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [kundel.component :as s]))

(deftest sometest
  (testing "nothing"
    (is (= true true))))
