(ns set1.1
  (:import (org.apache.commons.codec.binary Hex Base64)))

(->> "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
     (Hex/decodeHex)
     (Base64/encodeBase64)
     (String.))
;; => "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
