(ns set1.4
  (:require [clojure.string :as str]
            [set1.3 :refer [crack-1xor]])
  (:import (org.apache.commons.codec.binary Hex Base64)))

(let [file4 (slurp "https://cryptopals.com/static/challenge-data/4.txt")
      lines (str/split-lines file4)]
  (->> lines
       (map #(Hex/decodeHex %))
       (map #(crack-1xor %))
       (concat)
       (apply max-key :prob)
       :text))
;; => "Now that the party is jumping\n"
