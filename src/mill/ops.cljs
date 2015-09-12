(ns mill.ops
  (:require [mill.slice :refer [slice octet-seq from-octet-seq]]
            [mill.nar :refer [nar]]))

(defn addu
  "Unsigned integer addition."
  ;; TODO: Figure out if add is supposed to widen operands or only accept
  ;; equal-width operands. For now, assume same width
  ;; TODO: Figure out what happens if you try to perform a widening add with
  ;; the largest possible width operands?
  [overflow x y]
  (let [rev-bytes   (comp reverse octet-seq)
        pairs       (map vector (rev-bytes x) (rev-bytes y))
        [sum carry] (reduce
                      (fn [[sum carry] [a b]]
                        (let [byte-sum (+ a b carry)]
                          [(cons (mod byte-sum 256) sum) (quot byte-sum 256)]))
                      ['() 0]
                      pairs)
        overflowed? (= carry 1)
        to-value    (fn [byte-seq]
                      (with-meta
                        (from-octet-seq (:byte-width x) byte-seq)
                        {:valid? true}))]
    (case overflow
      :modulo
        (to-value sum)
      :saturating
        (if overflowed?
          (to-value (repeat (:byte-width x) 255))
          (to-value sum))
      :widening
        (if overflowed?
          (let [operand-width (:byte-width x)
                new-bytes (apply conj (cons 1 sum) (repeat (dec operand-width) 0))]
            (to-value new-bytes))
          (to-value sum))
      :excepting
        (if overflowed?
          nar
          (to-value sum)))))

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
