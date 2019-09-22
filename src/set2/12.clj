(ns set2.12
  (:require [utils :refer [pad-pkcs rand-bytes]]
            [set2.10 :refer [aes-cipher]]
            [set2.11 :refer [detect]])
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

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

(String. (byte-array sol12))
;; => "Rollin' in my 5.0\nWith my rag-top down so my hair can blow\nThe girlies on standby waving just to say hi\nDid you stop? No, I just drove by\n"
