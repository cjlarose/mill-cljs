(ns mill.ops
  (:require [mill.slice :refer [scalar?]]
            [mill.nar :refer [nar]]))

(extend-type js/Buffer
  cljs.core/ICounted
  (-count [this]
    (.-length this))
  cljs.core/ISeqable
  (-seq
    [this] (map #(aget this %) (range (.-length this))))
  cljs.core/IIndexed
  (-nth
    ([this i] (aget this i))
    ([this i default] (if-let [octet (aget this i)]
                        octet
                        default))))

; (defn add-vector-support
;   "Adds vector support to a given operation"
;   [op]
;   (fn [overflow x y]
;     (if (and (scalar? x) (scalar? y))
;       (let [result (op overflow (first (:elements x)) (first (:elements y))

(defn- addu-buffers
  "Adds two buffers. Returns the sum as an octet seq along with the carry
  amount"
  [x y]
  (let [rev-bytes (comp reverse seq)
        pairs     (map vector (rev-bytes x) (rev-bytes y))]
    (reduce
      (fn [[sum carry] [a b]]
        (let [byte-sum (+ a b carry)]
          [(cons (mod byte-sum 256) sum) (quot byte-sum 256)]))
      ['() 0]
      pairs)))

(defn addu
  "Unsigned integer addition."
  ;; TODO: Figure out what happens if you try to perform a widening add with
  ;; the largest possible width operands
  [overflow x y]
  {:pre [(= (:byte-width x) (:byte-width y))
         (= (count (:elements x)) (count (:elements y)))]}
  (let [->buffer (fn [octet-seq] (js/Buffer. (clj->js octet-seq)))
        element-pairs (map vector (:elements x) (:elements y))
        el-add (fn [[a b]]
                 (addu-buffers (:buffer a) (:buffer b)))
        results (map el-add element-pairs)]
    (if (#{:modulo :saturating :excepting} overflow)
      (let [f (fn [[sum carry]]
                (if (or (= overflow :modulo) (= carry 0))
                  {:valid? true
                   :buffer (->buffer sum)}
                  (case overflow
                    :saturating
                      {:valid? true
                       :buffer (->buffer (repeat (:byte-width x) 255))}
                    :excepting
                      {:valid? false
                       :buffer (->buffer (repeat (:byte-width x) 255))})))]
        {:byte-width (:byte-width x)
         :elements   (map f results)}))))

  ; (let [overflowed? (= carry 1)
  ;       to-value    (fn [byte-seq]
  ;                     {:valid? true
  ;                      :buffer (js/Buffer. (clj->js byte-seq))})]
  ;   (case overflow
  ;     :modulo
  ;       (to-value sum)
  ;     :saturating
  ;       (if overflowed?
  ;         (to-value (repeat (:byte-width x) 255))
  ;         (to-value sum))
  ;     :widening
  ;       (if overflowed?
  ;         (let [operand-width (:byte-width x)
  ;               new-bytes (apply conj (cons 1 sum) (repeat (dec operand-width) 0))]
  ;           (to-value new-bytes))
  ;         (to-value sum))
  ;     :excepting
  ;       (if overflowed?
  ;         {:valid? false :buffer (js/Buffer. #js [-1])} ; nar
  ;         (to-value sum)))))

; (defn adduv
;   "Unsigned integer vector pairwise addition"
;   [overflow x y]
;   {:pre [(not (or (:scalar x) (:scalar y)))
;          (.-length (:

; (defn addf [belt a b]
;   (let [lhs (belt-nth belt a)
;         rhs (belt-nth belt b)]
;     (cond
;       (or (nar? lhs) (nar? rhs))
;         [{:scalar? true :valid? false :value -1}]
;       (or (none? lhs) (none? rhs))
;         [{:scalar? true :valid? false :value 0}]
;       :else
;         [{:scalar? true :valid? true :value (+ (:value lhs) (:value rhs))}])))

;; (def-core-op add
;;   {:domains [:unsigned-integer :signed-integer :pointer
;;              :binary-floating-point :decimal-floating-point]
;;    :overflow [:modulo :saturating :excepting :widening]
;;    :rounding [:from-zero :to-even :to-negative-inf :to-positive-inf :to-zero]
;;    :signatures [[:exu-arg :exu-arg]]
;;    :scalarities [:scalar :vector]}
;;
;;   (addu
;;     [overflow exu1 exu2] '())
;;   (adds
;;     [overflow exu1 exu2] '())
;;   (addp
;;     [exu1 exu2] '())
;;   (addf
;;     [rounding exu1 exu2] '())
;;   (addd
;;     [rounding exu1 exu2] '()))
