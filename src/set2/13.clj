(ns set2.13
  (:require [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [utils :refer [rand-bytes ecb-encrypt]]
            [set2.11 :refer [detect]])
  (:import org.apache.commons.codec.net.URLCodec))

(defonce random-aes-key (rand-bytes 16))

(defonce codec (URLCodec.))

(defn url-decode [str] (.decode codec str))
(defn url-encode [str]
  (.encode codec str))

(defn decode-cookie [txt]
  (let [txt (url-decode txt)
        pairs (str/split txt #"\&")]
    (keywordize-keys
     (into {}
           (map #(str/split % #"\=") pairs)))))

(decode-cookie "foo=bar&baz=qux&zap=zazzle")
;; => {:foo "bar", :baz "qux", :zap "zazzle"}

(defn encode-cookie [m]
  (->> m
       (map (fn [[k v]]
              (str (name k) "=" (url-encode (str v)))))
       (str/join "&")))

(decode-cookie
 (encode-cookie {:email "foo@bar.com&role=admin"
                 :uid 10
                 :role "user"}))
;; => {:email "foo@bar.com", :role "user", :uid "10"}

(defn profile-for [email]
  (encode-cookie
   {:email email
    :uid 10
    :role "user"}))

(profile-for "AAAAAAAAA")
;; => "email=AAAAAAAAA&uid=10&role=user"
;; => "email=AAA&role=user&uid=10"

(def user (profile-for "foo@bar.com"))
;; email=foo%40bar.com&role=user&uid=28193

(def encrypted
  (->> user
       (.getBytes)
       (ecb-encrypt random-aes-key)))

(defn att-view [email]
  (->> email
       (profile-for)
       (.getBytes)
       (ecb-encrypt random-aes-key)))

(defn att-view2 [bs]
  (att-view (String. (byte-array bs))))

(detect att-view2)
;; => :ecb
