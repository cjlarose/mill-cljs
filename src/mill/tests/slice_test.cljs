(ns mill.tests.slice-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [mill.slice :as slice]))

(defn is-element [expected actual]
  (is (= (:valid? expected) (:valid? actual)))
  (is (.equals (js/Buffer. (clj->js (:buffer expected))) (:buffer actual))))

(deftest test-int-slice
  (testing "with single argument"
    (let [{:keys [byte-width elements]} (slice/int-slice 42)]
      (is (= 4 byte-width))
      (is (= 1 (count elements)))
      (is-element {:valid? true :buffer [0 0 0 42]} (first elements))))
  (testing "with multiple arguments"
    (let [{:keys [byte-width elements]} (slice/int-slice 65 97 256)
          expected-elements [{:valid? true :buffer [0 0 0 65]}
                             {:valid? true :buffer [0 0 0 97]}
                             {:valid? true :buffer [0 0 1 0]}]]
      (is (= 4 byte-width))
      (is (= 3 (count elements)))
      (doseq [[expected actual] (map vector expected-elements elements)]
        (is-element expected actual)))))

(run-tests)
