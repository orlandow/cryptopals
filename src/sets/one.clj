(ns sets.one
  (:import (org.apache.commons.codec.binary Hex Base64)
           javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec)
  (:require [clojure.string :as str]))

;; #1
;; -------------------------

(->> "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
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


;; #3
;; -------------------------

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

(defn crack-1xor* [bs]
  (letfn [(calc [ch]
            (->> (byte-array (count bs) (byte ch))
                 (map bit-xor bs)
                 (byte-array)
                 (String.)))]
    (->> (range 128)
         (map (fn [c]
                {:text (calc c)
                 :key c}))
         (map (fn [{:keys [text key]}]
                {:text text
                 :prob (english-prob text)
                 :key key}))
         (sort-by :prob #(compare %2 %1)))))

(defn crack-1xor [bs]
  (first (crack-1xor* bs)))

(let [input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      b1 (Hex/decodeHex input)]
  (->> (crack-1xor b1)
       :text))
;; => "Cooking MC's like a pound of bacon"


;; #4
;; -------------------------

(let [file4 (slurp "https://cryptopals.com/static/challenge-data/4.txt")
      lines (str/split-lines file4)]
  (->> lines
       (map #(Hex/decodeHex %))
       (map #(crack-1xor %))
       (mapcat #(max-key :prob %))
       (apply max-key :prob)
       :text))
;; => "Now that the party is jumping\n"

;; #5
;; -------------------------

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


;; #6
;; -------------------------

(defn bit-count* [b]
  (loop [b b
         count 0]
    (if (zero? b)
      count
      (recur
       (bit-shift-right b 1)
       (+ count (bit-and 1 b))))))

(def bit-counts
  (->>
   (range 128)
   (map byte)
   (map bit-count*)
   vec))

(defn hamming [bs1 bs2]
  (apply + (map (comp bit-counts bit-xor) bs1 bs2)))
;; => 37

(defn calc-keysize-prob [bs ksize]
  (let [blocks (partition ksize bs)
        blocks (take 10 blocks)
        ds (map (partial apply hamming) (partition 2 1 blocks))
        ds (map #(/ % ksize) ds)
        avg (/ (apply + ds) (count ds))]
    avg))

(defn guess-key-size [bs]
  (let [sizes (range 2 41)]
    (map (fn [k]
           {:prob (calc-keysize-prob bs k)
            :key-size k})
         sizes)))

(defonce file6 (slurp "https://cryptopals.com/static/challenge-data/6.txt"))
(def bytes6 (Base64/decodeBase64 (.getBytes file6)))

(let [probs (guess-key-size bytes6)
      keysizes (sort-by :prob probs)]
  (map :key-size
       (take 5 keysizes)))
;; => (29 5 3 15 2)

(def code
  (let [blocks (partition 29 bytes6)
        transposed (apply map vector blocks)]
    (->> (map crack-1xor transposed)
         (map :key))))

(String. (byte-array code))
;; => "Terminator X: Bring the noise"

(->> bytes6
     (map bit-xor (apply concat (repeat code)))
     (byte-array)
     (String.))
;; => "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n"


;; #7
;; -------------------------

(defonce file7 (slurp "https://cryptopals.com/static/challenge-data/7.txt"))
(def bytes7 (Base64/decodeBase64 (.getBytes file7)))

(def key7 "YELLOW SUBMARINE")

(def cipher
  (doto (Cipher/getInstance "AES")
    (.init Cipher/DECRYPT_MODE
           (SecretKeySpec. (.getBytes key7) "AES"))))

(String.
 (.doFinal cipher bytes7))
;; => "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n"
