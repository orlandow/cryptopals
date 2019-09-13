(ns sets.one
  (:import org.apache.commons.codec.binary.Hex
           org.apache.commons.codec.binary.Base64))

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

;; #2
;; -------------------------
