(ns mill.tests.ops-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [mill.tests.util :refer [is-element]]
            [mill.slice :as slice]
            [mill.ops :as ops]))

(deftest test-addu
  (testing "modulo"
    (testing "with scalars"
      (testing "without overflow"
        (let [sum    (ops/addu :modulo (slice/int-slice 65) (slice/int-slice 42))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 107]} (first elements))))
      (testing "with overflow"
        (let [sum    (ops/addu :modulo (slice/int-slice -1) (slice/int-slice 2))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 1]} (first elements)))))
    (testing "with vectors"
      (testing "without overflow"
        (let [sum    (ops/addu :modulo (slice/int-slice 65 97) (slice/int-slice 42 21))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 2 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 107]} (first elements))
          (is-element {:valid? true :buffer [0 0 0 118]} (second elements))))
      (testing "with overflow"
        (let [sum    (ops/addu :modulo (slice/int-slice 65 97) (slice/int-slice -1 21))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 2 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 64]} (first elements))
          (is-element {:valid? true :buffer [0 0 0 118]} (second elements)))))))

(run-tests)
