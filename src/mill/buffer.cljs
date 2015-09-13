(ns mill.buffer)

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

(defn addu-buffers
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

(defn ->buffer
  "Creates a Buffer from and octet seq"
  [octet-seq]
  (js/Buffer. (clj->js octet-seq)))

(defn widen-buffer
  "Returns a buffer of double width, zero-extending to the left"
  [buffer]
  (let [old-len (.-length buffer)
        new-buffer (js/Buffer. (* 2 old-len))]
    (.fill new-buffer 0)
    (.copy buffer new-buffer old-len)
    new-buffer))
