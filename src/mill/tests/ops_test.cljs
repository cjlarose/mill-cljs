(ns mill.tests.ops-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [mill.tests.util :refer [is-element is-element-nar]]
            [mill.slice :as slice]
            [mill.ops :as ops]))

(deftest test-addu
  ;; TODO: Make sure NaRs are carried though execution
  (testing "without overflow"
    (testing "with scalars"
      (doseq [overflow [:modulo :saturating :excepting :widening]]
        (let [sum    (ops/addu overflow (slice/int-slice 65) (slice/int-slice 42))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 107]} (first elements)))))
    (testing "with vectors"
      (testing "with modulo, saturating, and excepting overflow behavior"
        (doseq [overflow [:modulo :saturating :excepting]]
          (let [sum    (ops/addu overflow (slice/int-slice 65 97) (slice/int-slice 42 21))
                {:keys [byte-width elements]} sum]
            (is (= 4 byte-width))
            (is (= 2 (count elements)))
            (is-element {:valid? true :buffer [0 0 0 107]} (first elements))
            (is-element {:valid? true :buffer [0 0 0 118]} (second elements)))))
      (testing "with widening overflow behavior"
        (let [[l r] (ops/addu :widening (slice/int-slice 65 97) (slice/int-slice 42 21))]
          (is (= 4 (:byte-width l)))
          (is (= 4 (:byte-width r)))
          (is (= 1 (count (:elements l))))
          (is (= 1 (count (:elements r))))
          (is-element {:valid? true :buffer [0 0 0 107]} (first (:elements l)))
          (is-element {:valid? true :buffer [0 0 0 118]} (first (:elements r)))))))
  (testing "with overflow"
    (testing "modulo"
      (testing "with scalars"
        (let [sum    (ops/addu :modulo (slice/int-slice -1) (slice/int-slice 2))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 1]} (first elements))))
      (testing "with vectors"
        (let [sum    (ops/addu :modulo (slice/int-slice 65 97) (slice/int-slice -1 21))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 2 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 64]} (first elements))
          (is-element {:valid? true :buffer [0 0 0 118]} (second elements)))))
    (testing "saturating"
      (testing "with scalars"
        (let [sum    (ops/addu :saturating (slice/int-slice -1) (slice/int-slice 2))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [255 255 255 255]} (first elements))))
      (testing "with vectors"
        (let [sum    (ops/addu :saturating (slice/int-slice 65 97) (slice/int-slice -1 21))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 2 (count elements)))
          (is-element {:valid? true :buffer [255 255 255 255]} (first elements))
          (is-element {:valid? true :buffer [0 0 0 118]} (second elements)))))
    (testing "excepting"
      (testing "with scalars"
        (let [sum    (ops/addu :excepting (slice/int-slice -1) (slice/int-slice 2))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 1 (count elements)))
          (is-element-nar (first elements))))
      (testing "with vectors"
        (let [sum    (ops/addu :excepting (slice/int-slice 65 97) (slice/int-slice -1 21))
              {:keys [byte-width elements]} sum]
          (is (= 4 byte-width))
          (is (= 2 (count elements)))
          (is-element-nar (first elements))
          (is-element {:valid? true :buffer [0 0 0 118]} (second elements)))))
    (testing "widening"
      (testing "with scalars"
        (let [sum    (ops/addu :widening (slice/int-slice -1) (slice/int-slice 2))
              {:keys [byte-width elements]} sum]
          (is (= 8 byte-width))
          (is (= 1 (count elements)))
          (is-element {:valid? true :buffer [0 0 0 1 0 0 0 1]} (first elements))))
      (testing "with vectors"
        (let [[l r] (ops/addu :widening (slice/int-slice 65 97) (slice/int-slice -1 21))]
          (is (= 8 (:byte-width l)))
          (is (= 8 (:byte-width r)))
          (is (= 1 (count (:elements l))))
          (is (= 1 (count (:elements r))))
          (is-element {:valid? true :buffer [0 0 0 1 0 0 0 64]} (first (:elements l)))
          (is-element {:valid? true :buffer [0 0 0 0 0 0 0 118]} (first (:elements r))))))))

(run-tests)
