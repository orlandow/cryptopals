(ns set1.3
  (:require [utils :refer [english-prob]])
  (:import (org.apache.commons.codec.binary Hex)))

(defn crack-1xor* [bs]
  (letfn [(calc [ch]
            (->> (byte-array (count bs) (byte ch))
                 (map bit-xor bs)
                 (byte-array)
                 (String.)))]
    (->> (range 128)
         (map (fn [c]
                {:text (calc c)
                 :key c}))
         (map (fn [{:keys [text key]}]
                {:text text
                 :prob (english-prob text)
                 :key key}))
         (sort-by :prob #(compare %2 %1)))))

(defn crack-1xor [bs]
  (first (crack-1xor* bs)))

(let [input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      b1 (Hex/decodeHex input)]
  (->> (crack-1xor b1)
       :text))
;; => "Cooking MC's like a pound of bacon"
