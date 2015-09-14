(ns mill.ops
  (:require [mill.slice :refer [scalar? split-slice]]
            [mill.nar :refer [nar result]]
            [mill.unsigned-math :as u]
            [mill.ct :refer [bind]]
            [mill.buffer :refer [addu-buffers ->buffer widen-buffer]]))

; (defn add-vector-support
;   "Adds vector support to a given operation"
;   [op]
;   (fn [overflow x y]
;     (if (and (scalar? x) (scalar? y))
;       (let [result (op overflow (first (:elements x)) (first (:elements y))

(defn widenu
  "Doubles the width of a unsigned integer scalar, zero-extending to the left.
  For a vector argument, produces two result vectors"
  [s]
  (let [new-elements (map #(update % :buffer widen-buffer) (:elements s))
        new-byte-width (* 2 (:byte-width s))
        result {:byte-width new-byte-width :elements new-elements}]
    (if (scalar? s)
      result
      (split-slice result))))

(defn addu
  "Unsigned integer addition."
  ;; TODO: Figure out what happens if you try to perform a widening add with
  ;;       the largest possible width scalar operands
  ;; TODO: If an adduwv op causes only one of the result vectors to widen
  ;;       Are both results supposed to widen?
  ;; TODO: Make sure NaRs are carried though execution
  [overflow x y]
  {:pre [(= (:byte-width x) (:byte-width y))
         (= (count (:elements x)) (count (:elements y)))]}
    (if (#{:modulo :saturating :excepting} overflow)
      (let [f {:modulo u/addu
               :saturating u/addus
               :excepting u/addux}
            g (fn [a b]
                (bind a (fn [i] (bind b (fn [j] ((f overflow) i j))))))]
        {:byte-width (:byte-width x)
         :elements   (map g (:elements x) (:elements y))})
      ; (= overflow :widening)
      (let [el-add (fn [a b]
                     (addu-buffers (:value a) (:value b)))
            results (map el-add (:elements x) (:elements y))
            wide-el (fn [sum carry]
                      (let [new-bytes (apply conj (cons carry sum) (repeat (dec (:byte-width x)) 0))]
                        (result (->buffer new-bytes))))]
        (if (scalar? x)
          (let [[sum carry] (first results)]
            (if (= carry 0)
              {:byte-width (:byte-width x)
               :elements   [(result (->buffer sum))]}
              {:byte-width (* 2 (:byte-width x))
               :elements   [(wide-el sum 1)]}))
          (let [overflowed? (some (fn [[_ carry]] (not= carry 0)) results)]
            (if overflowed?
              (split-slice
                {:byte-width (* 2 (:byte-width x))
                 :elements   (map (fn [[sum carry]] (wide-el sum carry)) results)})
              (split-slice
                {:byte-width (:byte-width x)
                 :elements   (map (fn [[sum _]] (result (->buffer sum))) results)})))))))

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
