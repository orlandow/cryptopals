(ns utils
  (:require [clojure.string :as str])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

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

(defn pad-pkcs [n bs]
  (let [x (mod (- (count bs)) n)]
    (concat bs
            (repeat x (byte x)))))

(defn rand-bytes [n]
  (byte-array
   (repeatedly n #(rand-int 128))))

(defn ecb-cipher [key encrypt?]
  (let [bs (if (instance? String key)
             (.getBytes key)
             (byte-array key))
        mode (if encrypt? Cipher/ENCRYPT_MODE Cipher/DECRYPT_MODE)]
    (doto (Cipher/getInstance "AES/ECB/PKCS5Padding")
      (.init mode (SecretKeySpec. bs "AES")))))

(defn ecb-encrypt [key bs]
  (let [cipher (ecb-cipher key true)]
    (.doFinal cipher bs)))

(defn ecb-decrypt [key bs]
  (let [cipher (ecb-cipher key false)]
    (.doFinal cipher bs)))
