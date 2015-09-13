(ns mill.slice)

; (defn slice
;   "Makes a new, zeroed-out slice of the given byte-width and length"
;   ([length byte-width]
;    (slice length byte-width 0))
;   ([length byte-width fill-value]
;    {:byte-width byte-width
;     :buffer (doto (js/Buffer. (* length byte-width))
;               (.fill fill-value))}))

(defn scalar?
  "Returns whether the slice is a scalar. Scalars in Mill are just
  vectors of length 1"
  [{:keys [elements]}]
  (= (count elements) 1))

(defn- valid-int? [x]
  (and
    (js/Number.isSafeInteger x)
    (< x (js/Math.pow 2 31))
    (>= x (- (js/Math.pow 2 31)))))

(defn int-slice
  "Produces a vector of byte-width 4 for the given seq of integers"
  [& xs]
  {:pre [(every? valid-int? xs)]}
  {:byte-width 4
   :elements (map (fn [x] {:valid? true
                           :buffer (doto
                                     (js/Buffer. 4)
                                     (.writeInt32BE x 0))}) xs)})

(defn split-slice
  [{:keys [byte-width elements]}]
  (let [halves (split-at (/ (count elements) 2) elements)]
    (mapv (fn [v] {:byte-width byte-width :elements v}) halves)))

; (defn from-octet-seq [byte-width octet-seq]
;   {:byte-width byte-width
;    :buffer (js/Buffer. (clj->js octet-seq))})
; 
; (defn octet-seq
;   "Returns the list of octets of a slice"
;   [{:keys [buffer]}]
;   (map #(aget buffer %) (range (.-length buffer))))
; 
; (defn- elements
;   "Returns a seq of buffers representing elements of the given slice"
;   [{:keys [buffer byte-width]}]
;   (map
;     (fn [offset] (.slice buffer offset (+ offset byte-width)))
;     (range 0 (.-length buffer) byte-width)))
