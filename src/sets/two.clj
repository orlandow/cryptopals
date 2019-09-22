(ns sets.two
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

;; challenge 9
;; -----------------------------------------

(defn pad-pkcs [n bs]
  (let [x (mod (- (count bs)) n)]
    (concat bs
            (repeat x (byte x)))))

(->> "YELLOW SUBMARINE"
     (.getBytes)
     (pad-pkcs 20))
;; => (89 69 76 76 79 87 32 83 85 66 77 65 82 73 78 69 4 4 4 4)

;; challenge 10
;; -----------------------------------------

(defonce file10 (slurp "https://cryptopals.com/static/challenge-data/10.txt"))

(def key10 "YELLOW SUBMARINE")
(def size10 16)
(def iv10 (byte-array 16 (byte 0)))
(def initial10 "This is a string longer than 16 bytes")
(def bytes10
  (->> file10
       (.getBytes)
       (Base64/decodeBase64)))

(defn aes-cipher [mode key]
  (let [bs (if (instance? String key)
             (.getBytes key)
             (byte-array key))]
    (doto (Cipher/getInstance "AES/ECB/NoPadding")
      (.init mode
             (SecretKeySpec. bs "AES")))))

(defn cbc [bs iv key size]
  (let [cipher (aes-cipher Cipher/ENCRYPT_MODE key)]
    (->> bs
         (pad-pkcs size)
         (partition size)
         (reduce (fn [[acc lastc] b]
                   (let [b2 (map bit-xor b lastc)
                         b2enc (.doFinal cipher (byte-array b2))]
                     [(concat acc b2enc)
                      b2enc]))
                 [[] iv])
         (first)
         (byte-array))))

(defn cbc-1 [bs iv key size]
  (let [cipher (aes-cipher Cipher/DECRYPT_MODE key)]
    (->> bs
         (partition size)
         (reduce (fn [[acc lastc] b]
                   (let [b2dec (.doFinal cipher (byte-array b))
                         b2 (map bit-xor b2dec lastc)]
                     [(concat acc b2)
                      b]))
                 [[] iv])
         (first)
         (byte-array))))

(def res1
  (cbc (.getBytes initial10) iv10 key10 size10))

(cbc-1 res1 iv10 key10 size10)
(String. *1)
;; => "This is a string longer than 16 bytes"

(cbc-1 bytes10 iv10 key10 size10)
(String. *1)
;; => "I'm back and I'm ringin' the bell \nA rockin' on the mike while...

;; challenge 11
;; -----------------------------------------

(defn rand-bytes [n]
  (byte-array
   (repeatedly n #(rand-int 128))))

(defn random-aes-key []
  (rand-bytes 16))

(random-aes-key)
;; => #object["[B" 0xb49ce0e "[B@b49ce0e"]
;; => (13 1 118 45 41 123 120 120 24 81 94 104 118 88 66 85)

(defn ecb11 [bs key]
  (let [cipher (aes-cipher Cipher/ENCRYPT_MODE key)]
    (.doFinal cipher (byte-array bs))))

(defn cbc11 [bs key]
  (let [iv (rand-bytes 16)]
    (cbc bs iv key 16)))

(defn encryption-oracle
  ([bs]
   (encryption-oracle bs (rand-nth [:ecb :cbc])))
  ([bs mode]
   (let [k (random-aes-key)
         bs (map byte (concat (rand-bytes (+ 5 (rand-int 6)))
                              bs
                              (rand-bytes (+ 5 (rand-int 6)))))
         bs (pad-pkcs 16 bs)]
     (case mode
       :cbc (cbc11 bs k)
       :ecb (ecb11 bs k)))))

(defonce txt11 (slurp "https://www.gutenberg.org/cache/epub/174/pg174.txt"))

(defn random-text []
  (let [start (+ 200 (rand-int 1000))
        length (+ 20 (rand-int 80))]
    (-> txt11
        (subs start (+ start length))
        (.getBytes))))

(defn detect [f]
  (let [prob (->> (f (repeat 512 (byte 10)))
                  (frequencies)
                  (vals)
                  (apply max))]
    (if (> prob 20)
      :ecb
      :cbc)))

(def cbc-er #(encryption-oracle % :cbc))
(def ecb-er #(encryption-oracle % :ecb))

(repeatedly 20 #(detect cbc-er))
;; => (:cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc)
;; => (:cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc)
;; => (:cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc :cbc)

(repeatedly 20 #(detect ecb-er))
;; => (:ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb)
;; => (:ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb)
;; => (:ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb :ecb)


;; challenge 12
;; -----------------------------------------

(def str12 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
YnkK")

(defonce key12 (rand-bytes 16))

(defn ecb12 [bs]
  (let [cipher (aes-cipher Cipher/ENCRYPT_MODE key12)
        extra (Base64/decodeBase64 str12)
        bs (concat bs extra)
        bs (pad-pkcs 16 bs)]
    (.doFinal cipher (byte-array bs))))

(def all-bytes (map byte (range 128)))

(def aaa
  (for [i (range 256)]
    (repeat i (byte \A))))

(map (comp seq ecb12) aaa)

;; Block size = 16

(detect ecb12)
;; => :ecb

(def aaax
  (take 16 (ecb12 (repeat 15 (byte \A)))))
;; => (-37 -13 -65 -103 -97 18 -125 60 112 -40 -109 -33 46 88 9 -35)

(def possible-bytes
  (into {}
        (for [b (byte-array (range 128))]
          [(take 16 (ecb12 (concat (repeat 15 (byte \A)) [b]))) b])))

(get possible-bytes aaax)
;; => 82

(def aanx
  (take 16 (ecb12 (repeat 14 (byte \A)))))
;; => (34 38 56 17 109 -116 57 -75 110 98 15 -40 11 126 75 -54)

(def possible-bytes2
  (into {}
        (for [b (byte-array (range 128))]
          [(take 16 (ecb12 (concat (repeat 14 (byte \A)) [82 b]))) b])))

(get possible-bytes2 aanx)
;; => 111

;;     ?
;; |AAA1|2345| <---
;; |AA12|3456|
;; |A123|4567|
;; |1234|5678| <---
;;          ?
;; |AAA1|2345| <---
;; |AA12|3456| <---

(defn crack-next [f block-size known]
  (let [known-size (count known)
        offset (mod (- -1 known-size) block-size)
        aaa (repeat offset (byte \A))
        aaa+known (concat aaa known)
        block-count (inc (quot known-size block-size))
        full-length (* block-count block-size)
        sol-1 (take full-length (f aaa))
        possible (into {}
                       (for [b all-bytes]
                         [(take full-length (f (concat aaa+known [b])))
                          b]))]
    (or (get possible sol-1)
        :not-found)))

(crack-next ecb12 16 [82])
;; => 111
;; => 82

(def sol12
  (nth
   (iterate (fn [acc]
              (conj acc (crack-next ecb12 16 acc))) [])
   (count (Base64/decodeBase64 str12))))
;; => "Rollin' in my 5.0\nWith my rag-top down so my hair can blow\nThe girlies on standby waving just to say hi\nDid you stop? No, I just drove by\n"
