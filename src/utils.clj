(ns utils
  (:require [clojure.string :as str]))

(defn english-prob [txt]
  (let [common "etaoin shrdlu"
        txt (str/lower-case txt)]
    (->> common
         (reduce
          (fn [[acc i] c]
            (let [matches (re-seq (re-pattern (str c)) txt)]
              [(+ acc (* (/ 1.0 (+ 10. i)) (double (count matches)))) (+ 1.0 i)]))
          [0.0 1.0])
         (first))))

(defn pad-pkcs [n bs]
  (let [x (mod (- (count bs)) n)]
    (concat bs
            (repeat x (byte x)))))

(defn rand-bytes [n]
  (byte-array
   (repeatedly n #(rand-int 128))))
