(ns set1.6
  (:require [set1.3 :refer [crack-1xor]])
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(defn bit-count* [b]
  (loop [b b
         count 0]
    (if (zero? b)
      count
      (recur
       (bit-shift-right b 1)
       (+ count (bit-and 1 b))))))

(def bit-counts
  (->>
   (range 128)
   (map byte)
   (map bit-count*)
   vec))

(defn hamming [bs1 bs2]
  (apply + (map (comp bit-counts bit-xor) bs1 bs2)))
;; => 37

(defn calc-keysize-prob [bs ksize]
  (let [blocks (partition ksize bs)
        blocks (take 10 blocks)
        ds (map (partial apply hamming) (partition 2 1 blocks))
        ds (map #(/ % ksize) ds)
        avg (/ (apply + ds) (count ds))]
    avg))

(defn guess-key-size [bs]
  (let [sizes (range 2 41)]
    (map (fn [k]
           {:prob (calc-keysize-prob bs k)
            :key-size k})
         sizes)))

(defonce file6 (slurp "https://cryptopals.com/static/challenge-data/6.txt"))
(def bytes6 (Base64/decodeBase64 (.getBytes file6)))

(let [probs (guess-key-size bytes6)
      keysizes (sort-by :prob probs)]
  (map :key-size
       (take 5 keysizes)))
;; => (29 5 3 15 2)

(def code
  (let [blocks (partition 29 bytes6)
        transposed (apply map vector blocks)]
    (->> (map crack-1xor transposed)
         (map :key))))

(String. (byte-array code))
;; => "Terminator X: Bring the noise"

(->> bytes6
     (map bit-xor (apply concat (repeat code)))
     (byte-array)
     (String.))
;; => "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ec...
