(ns set2.09)

(defn pad-pkcs [n bs]
  (let [x (mod (- (count bs)) n)]
    (concat bs
            (repeat x (byte x)))))

(->> "YELLOW SUBMARINE"
     (.getBytes)
     (pad-pkcs 20))
;; => (89 69 76 76 79 87 32 83 85 66 77 65 82 73 78 69 4 4 4 4)
