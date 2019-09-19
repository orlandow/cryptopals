(ns sets.one
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec)
  (:require [clojure.string :as str]))

;; #1
;; -------------------------

(->> "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
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


;; #4
;; -------------------------

(let [file4 (slurp "https://cryptopals.com/static/challenge-data/4.txt")
      lines (str/split-lines file4)]
  (->> lines
       (map #(Hex/decodeHex %))
       (map #(crack-1xor %))
       (concat)
       (apply max-key :prob)
       :text))
;; => "Now that the party is jumping\n"

;; #5
;; -------------------------

(defn encrypt-repeating-xor [bs k]
  (let [keys (cycle k)]
    (map bit-xor bs keys)))

(let [text "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"
      k (map byte "ICE")
      bs (.getBytes text)]
  (->> (encrypt-repeating-xor bs k)
       (byte-array)
       (Hex/encodeHex)
       (String.)))
;; => "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"


;; #6
;; -------------------------

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


;; #7
;; -------------------------

(defonce file7 (slurp "https://cryptopals.com/static/challenge-data/7.txt"))
(def bytes7 (Base64/decodeBase64 (.getBytes file7)))

(def key7 "YELLOW SUBMARINE")

(def cipher
  (doto (Cipher/getInstance "AES")
    (.init Cipher/DECRYPT_MODE
           (SecretKeySpec. (.getBytes key7) "AES"))))

(String.
 (.doFinal cipher bytes7))
;; => "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ...

;; #8
;; -------------------------

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
