(ns kundel.smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [kundel.component :as s]))

(deftest get-narration-basic
  (let [sections '({:flows (:a :b :c)
                    :subsections ({:flows (:x :y :z)}
                                  {:flows (:o)}
                                  {:flows (:l :m :n)})}
                   {:flows (:e :f :g)})]
    (testing "is a vanilla section setup creating the right narration"
      (let [result (s/get-narration sections)]
        (is (= result
               '(
                 {:section
                             {:flows (:a :b :c),
                              :subsections
                                     ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection nil,
                  :flow :a,
                  :subsnext nil,
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection nil,
                  :flow :b,
                  :subsnext nil,
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection nil,
                  :flow :c,
                  :subsnext nil,
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:x :y :z)},
                  :flow :x,
                  :subsnext {:flows (:o)},
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:x :y :z)},
                  :flow :y,
                  :subsnext {:flows (:o)},
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:x :y :z)},
                  :flow :z,
                  :subsnext {:flows (:o)},
                  :subsprev nil}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:o)},
                  :flow :o,
                  :subsnext {:flows (:l :m :n)},
                  :subsprev {:flows (:x :y :z)}}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:l :m :n)},
                  :flow :l,
                  :subsnext nil,
                  :subsprev {:flows (:o)}}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:l :m :n)},
                  :flow :m,
                  :subsnext nil,
                  :subsprev {:flows (:o)}}
                 {:section
                              {:flows (:a :b :c),
                               :subsections
                                      ({:flows (:x :y :z)} {:flows (:o)} {:flows (:l :m :n)})},
                  :subsection {:flows (:l :m :n)},
                  :flow :n,
                  :subsnext nil,
                  :subsprev {:flows (:o)}}
                 {:section {:flows (:e :f :g)},
                  :subsection nil,
                  :flow :e,
                  :subsnext nil,
                  :subsprev nil}
                 {:section {:flows (:e :f :g)},
                  :subsection nil,
                  :flow :f,
                  :subsnext nil,
                  :subsprev nil}
                 {:section {:flows (:e :f :g)},
                  :subsection nil,
                  :flow :g,
                  :subsnext nil,
                  :subsprev nil})))))))

(deftest get-narration-no-flows-just-subsections
  (let [sections '({:subsections ({:flows (:x)})}
                   {:flows (:e)})]
    (testing "is a section setup without flows but with subsections OK?"
      (let [result (s/get-narration sections)]
        (is (= result
               '({:section {:subsections ({:flows (:x)})},
                  :subsection {:flows (:x)},
                  :flow :x,
                  :subsnext nil,
                  :subsprev nil}
                 {:section {:flows (:e)},
                  :subsection nil,
                  :flow :e,
                  :subsnext nil,
                  :subsprev nil})))))))

(deftest get-narration-one-flow
  (let [sections '({:flows (:e)})]
    (testing "is a section setup with just one flow OK?"
      (let [result (s/get-narration sections)]
        (is (= result
               '({:section {:flows (:e)},
                  :subsection nil,
                  :flow :e,
                  :subsnext nil,
                  :subsprev nil})))))))

(deftest get-narration-empty
  (let [sections '()]
    (testing "is a section setup with just one flow OK?"
      (let [result (s/get-narration sections)]
        (is (= result
               nil))))))

(deftest get-narration-keyframe-basic
  (let [sections '({:flows (:a :b :c)
                    :subsections ({:flows (:x :y :z)}
                                  {:flows (:o)}
                                  {:flows (:l :m :n)})}
                   {:flows (:e :f :g)})
        narration (s/get-narration sections)]
    (testing "ensure we can get keyframe of flows"
        (is (= 0 (s/get-narration-keyframe narration :a)))
        (is (= 3 (s/get-narration-keyframe narration :x)))
        (is (= 6 (s/get-narration-keyframe narration :o))))))
