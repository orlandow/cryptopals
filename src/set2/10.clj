(ns set2.10
  (:require [utils :refer [pad-pkcs]])
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

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
