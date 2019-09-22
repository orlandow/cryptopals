(ns set1.5
  (:import (org.apache.commons.codec.binary Hex)))


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
