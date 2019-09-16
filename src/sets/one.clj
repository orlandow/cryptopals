(ns sets.one
  (:import org.apache.commons.codec.binary.Hex
           org.apache.commons.codec.binary.Base64)
  (:require [clojure.string :as str]))

;; #1
;; -------------------------

(->> "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
     (.toCharArray)
     (Hex/decodeHex)
     (Base64/encodeBase64)
     (String.))
;; => "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"


;; #2
;; -------------------------

(let [b1 "1c0111001f010100061a024b53535009181c"
      b2 "686974207468652062756c6c277320657965"
      h1 (Hex/decodeHex b1)
      h2 (Hex/decodeHex b2)]
  (->> (map bit-xor h1 h2)
       (byte-array)
       (Hex/encodeHex)
       (String.)))
;; => "746865206b696420646f6e277420706c6179"


;; #3
;; -------------------------

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

(defn crack-1xor [bs]
  (letfn [(calc [ch]
            (->> (byte-array (count bs) (byte ch))
                 (map bit-xor bs)
                 (byte-array)
                 (String.)))]
    (->> (range 128)
         (map calc)
         (map (fn [txt]
                {:text txt
                 :prob (english-prob txt)}))
         (sort-by :prob #(compare %2 %1))
         (take 5))))

(let [input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      b1 (Hex/decodeHex input)]
  (->> (crack-1xor b1)
       first
       :text))
;; => "Cooking MC's like a pound of bacon"


;; #4
;; -------------------------

(let [file4 (slurp "https://cryptopals.com/static/challenge-data/4.txt")
      lines (str/split-lines file4)]
  (->> lines
       (map #(Hex/decodeHex %))
       (map #(crack-1xor %))
       (mapcat #(max-key :prob %))
       (apply max-key :prob)
       :text))
;; => "Now that the party is jumping\n"
