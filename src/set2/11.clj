(ns set2.11
  (:require [set2.10 :refer [cbc aes-cipher]]
            [utils :refer [pad-pkcs]])
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

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
