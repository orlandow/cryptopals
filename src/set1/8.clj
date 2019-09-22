(ns set1.8
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec)
  (:require [clojure.string :as str]))

(defonce file8 (slurp "https://cryptopals.com/static/challenge-data/8.txt"))

(def blocks8
  (->> (str/split-lines file8)
       (map #(Hex/decodeHex %))))

(defn ecbd-prob [bs]
  (->> (frequencies bs)
       (filter #(> (second %) 1))
       (map second)
       (apply +)))

(->> blocks8
     (map #(partition 16 %))
     (map-indexed (fn [i b] {:prob (ecbd-prob b)
                             :block b
                             :line (inc i)}))
     (sort-by :prob)
     last
     :line)
;; => 133


(->> blocks8
     (map-indexed (fn [i b] {:prob (ecbd-prob b)
                             :block b
                             :line (inc i)}))
     (sort-by :prob)
     last
     :line)
;; => 133
