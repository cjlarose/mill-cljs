(ns mill.core
  (:require [clojure.browser.repl :as repl]))

(def tin
  {:belt-size 8
   :max-scalar-size 8
   :max-operand-size 8})

(defn machine [{:keys [belt-size] :as family-member}]
  {:belt (repeat belt-size :none)
   :family-member family-member})

(defn execute-op [{:keys [belt family-member] :as m} op & args]
  (->> (apply op belt args)
       (apply conj belt)
       (take (:belt-size family-member))
       (assoc m :belt)))

(defn none? [operand]
  (and (not (:valid? operand)) (= (:value operand) 0)))

(defn nar? [operand]
  (and (not (:valid? operand)) (not= (:value operand) 0)))

(defn signed-scalar
  "Produces a belt scalar of width 4 for the given integer"
  [x]
  {:pre [(js/Number.isSafeInteger x)
         (< x (js/Math.pow 2 31))
         (>= x (- (js/Math.pow 2 31)))]}
  (let [buffer (doto (js/Buffer. 4) (.writeIntBE x 0 4))]
    {:scalar? true :valid? true :value buffer}))

(def nar (assoc (signed-scalar -1) :valid? false))

(defn widen
  "Doubles the width of an unsigned integer, zero-extending to the left"
  [{:keys [value] :as operand}]
  (let [old-length (.-length value)
        new-buffer (doto (js/Buffer. (* 2 old-length)) (.fill 0))]
    (.copy value new-buffer old-length)
    (assoc operand :value new-buffer)))

(defn buffer->byte-seq
  "Gets the octets of a Buffer as a list"
  [buffer]
  (map #(aget buffer %) (range (.-length buffer))))

(defn byte-seq->buffer [byte-seq]
  (js/Buffer. (clj->js byte-seq)))

(defn addu
  "Unsigned integer addition."
  ;; TODO: Figure out if add is supposed to widen operands or only accept
  ;; equal-width operands. For now, assume same width
  ;; What happens if you try to perform a widening add with the largest
  ;; possible width operands?
  [overflow x y]
  (let [rev-bytes   (comp reverse buffer->byte-seq :value)
        pairs       (map vector (rev-bytes x) (rev-bytes y))
        [sum carry] (reduce
                      (fn [[sum carry] [a b]]
                        (let [byte-sum (+ a b carry)]
                          [(cons (mod byte-sum 256) sum) (quot byte-sum 256)]))
                      ['() 0]
                      pairs)
        overflowed? (= carry 1)
        to-value    (fn [byte-seq]
                      {:scalar? true
                       :valid? true
                       :value (byte-seq->buffer byte-seq)})]
    (case overflow
      :modulo
        (to-value sum)
      :saturating
        (if overflowed?
          (to-value (take (.-length (:value x)) (repeat 255)))
          (to-value sum))
      :widening
        (if overflowed?
          (let [operand-width (.-length (:value x))
                new-bytes (apply conj (cons 1 sum) (repeat (dec operand-width) 0))]
            (to-value new-bytes))
          (to-value sum))
      :excepting
        (if overflowed?
          nar
          (to-value sum)))))

(defn belt-nth [belt pos-sym]
  (->> pos-sym
       (name)
       (drop 1)
       (apply str)
       (js/parseInt)
       (nth belt)))

(defn addf [belt a b]
  (let [lhs (belt-nth belt a)
        rhs (belt-nth belt b)]
    (cond
      (or (nar? lhs) (nar? rhs))
        [{:scalar? true :valid? false :value -1}]
      (or (none? lhs) (none? rhs))
        [{:scalar? true :valid? false :value 0}]
      :else
        [{:scalar? true :valid? true :value (+ (:value lhs) (:value rhs))}])))

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

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello world!")
