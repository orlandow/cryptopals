(ns set1.7
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

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
