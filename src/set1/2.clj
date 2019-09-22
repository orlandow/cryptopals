(ns set1.2
  (:import (org.apache.commons.codec.binary Hex Base64)))

(let [b1 "1c0111001f010100061a024b53535009181c"
      b2 "686974207468652062756c6c277320657965"
      h1 (Hex/decodeHex b1)
      h2 (Hex/decodeHex b2)]
  (->> (map bit-xor h1 h2)
       (byte-array)
       (Hex/encodeHex)
       (String.)))
;; => "746865206b696420646f6e277420706c6179"
