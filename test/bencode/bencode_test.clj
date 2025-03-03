;; Copyright (c) Meikel Brandmeyer. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.


(ns bencode.bencode-test
  (:require [clojure.test :refer [are deftest is testing]]
            [bencode.core :as bencode :refer [read-bencode
                                              read-netstring
                                              write-bencode
                                              write-netstring]])
  (:import clojure.lang.RT
           System.IO.MemoryStream, clojure.lang.PushbackInputStream))    ;;; [java.io ByteArrayInputStream ByteArrayOutputStream PushbackInputStream]

(defn #^{:private true} >bytes
  [#^String input]
  (.GetBytes System.Text.Encoding/UTF8 input))                           ;;; (.getBytes input "UTF-8"))

(defmulti #^{:private true} <bytes class)

(defmethod <bytes :default
  [input]
  input)

(defmethod <bytes |System.Byte[]|                        ;;; (RT/classForName "[B")
  [#^|System.Byte[]| input]                              ;;; #^"[B"
  (.GetString System.Text.Encoding/UTF8 input))          ;;; (String. input "UTF-8"))

(defmethod <bytes clojure.lang.IPersistentVector
  [input]
  (vec (map <bytes input)))

(defmethod <bytes clojure.lang.IPersistentMap
  [input]
  (->> input
       (map (fn [[k v]] [k (<bytes v)]))
       (into {})))

(defn- decode
  [bytes & {:keys [reader]}]
  (-> bytes
    MemoryStream.                                        ;;; ByteArrayInputStream.
      PushbackInputStream.
      reader))

(defn- >input
  [^String input & args]
  (-> (.GetBytes System.Text.Encoding/UTF8 input)              ;;; input
                                                               ;;; (.getBytes "UTF-8")
      (#(apply decode % args))
      <bytes))

(deftest test-netstring-reading
  (are [x y] (= (>input x :reader read-netstring) y)
    "0:,"                ""
    "13:Hello, World!,"  "Hello, World!"
    "16:Hällö, Würld!,"  "Hällö, Würld!"
    "25:Здравей, Свят!," "Здравей, Свят!"))

(deftest test-string-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "0:"                ""
    "13:Hello, World!"  "Hello, World!"
    "16:Hällö, Würld!"  "Hällö, Würld!"
    "25:Здравей, Свят!" "Здравей, Свят!"))

(deftest test-integer-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "i0e"     0
    "i42e"   42
    "i-42e" -42))

(deftest test-list-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "le"                    []
    "l6:cheesee"            ["cheese"]
    "l6:cheese3:ham4:eggse" ["cheese" "ham" "eggs"]))

(deftest test-map-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "de"            {}
    "d3:ham4:eggse" {"ham" "eggs"}))

(deftest test-nested-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "l6:cheesei42ed3:ham4:eggsee" ["cheese" 42 {"ham" "eggs"}]
    "d6:cheesei42e3:haml4:eggsee" {"cheese" 42 "ham" ["eggs"]}))

(defn- >stream
  [thing & {:keys [writer]}]
  (doto (MemoryStream.)                                         ;;; (ByteArrayOutputStream.)
    (writer thing)))

(defn- >output
  [& args]
  (-> >stream
      ^MemoryStream (apply args)                                ;;; ByteArrayOutputStream
      (#(.GetString System.Text.Encoding/UTF8 (.ToArray %)))))  ;;; (.toString "UTF-8")   had to add the .ToArray and also wrap as fn because the GetString takes args in the 'wrong' order.

(deftest test-netstring-writing
  (are [x y] (= (>output (>bytes x) :writer write-netstring) y)
    ""               "0:,"
    "Hello, World!"  "13:Hello, World!,"
    "Hällö, Würld!"  "16:Hällö, Würld!,"
    "Здравей, Свят!" "25:Здравей, Свят!,"))

(deftest test-byte-array-writing
  (are [x y] (= (>output (>bytes x) :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-string-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-input-stream-writing
  (are [x y] (= (>output (MemoryStream. (>bytes x))                     ;;; ByteArrayInputStream.
                         :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-integer-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    0 "i0e"
    42 "i42e"
    -42 "i-42e"

    ;; Works for all integral types.
    ;; Note: BigInts (42N) not tested, since they are not
    ;; supported in 1.2.
    (Byte/Parse "42")   "i42e"                                   ;;; Byte/parseByte  10
    (Int16/Parse "42") "i42e"                                    ;;; Short/parseShort 10
    (Int32/Parse "42") "i42e"                                    ;;; Integer/parseInt 10
    (Int64/Parse "42")   "i42e"))                                ;;; Long/parseLong 10

(deftest test-named-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    :foo      "3:foo"
    :foo/bar  "7:foo/bar"
    'foo      "3:foo"
    'foo/bar  "7:foo/bar"))

(deftest test-list-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    nil                     "le"
    []                      "le"
    ["cheese"]              "l6:cheesee"
    ["cheese" "ham" "eggs"] "l6:cheese3:ham4:eggse"))

(deftest test-map-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    {}             "de"
    {"ham" "eggs"} "d3:ham4:eggse"
    {:ham "eggs"}  "d3:ham4:eggse"
    {'ham "eggs"}  "d3:ham4:eggse"
    {:h/am "eggs"} "d4:h/am4:eggse"))

(deftest test-nested-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    ["cheese" 42 {"ham" "eggs"}] "l6:cheesei42ed3:ham4:eggsee"
    {"cheese" 42 "ham" ["eggs"]} "d6:cheesei42e3:haml4:eggsee"))

(deftest test-lexicographic-sorting
  (let [source   ["ham" "eggs" "hamburg" "hamburger" "cheese"]
        expected ["cheese" "eggs" "ham" "hamburg" "hamburger"]
        to-test  (->> source
                      (map >bytes)
                      (sort @#'bencode.core/lexicographically)     ;;; nrepl.bencode
                      (map <bytes))]
    (is (= to-test expected))))

(deftest unencoded-values
  ;; just some PNG data that won't round-trip cleanly through UTF-8 encoding, so
  ;; any default encoding in the bencode implementation will be caught immediately                    ;;; - negatives replaced because Byte not SByte.
  (let [binary-data (->> [119 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0                         ;;; -119 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0     
                          100 0 0 0 100 8 6 0 0 0 112 30 107 84 0 0 3 16 105                          ;;; 100 0 0 0 100 8 6 0 0 0 112 -30 -107 84 0 0 3 -16 105  
                          67 67 80 73 67 67 32 80 114 111 102 105 108 101 0 0 40                      ;;; 67 67 80 73 67 67 32 80 114 111 102 105 108 101 0 0 40  
                          111 115 85 35 111 37 84 20 63 119 111 92 92 22 63                           ;;; -111 -115 85 -35 111 -37 84 20 63 -119 111 92 -92 22 63 
                          96 79 114 14 21 117 81 85 83 91 71 27 26 83 58 6                            ;;; -96 -79 -114 14 21 -117 -81 85 83 91 -71 27 26 -83 -58 6
                          73 109 91 23 66 26 71 51 40 42 92 55 117 110]                               ;;; 73 -109 -91 -23 66 26 -71 -51 -40 42 -92 -55 117 110
                         (map byte)
                      (into-array System.Byte))]                             ;;; Byte/TYPE
    (is (= (seq binary-data)
           (-> {"data" binary-data}
               (>stream :writer write-bencode)
               .ToArray                                                      ;;; .toByteArray
               (decode :reader read-bencode)
               (get "data")
               seq)))))

#_(deftest unwritable-values
  (testing "write-bencode writes eagerly"
    (let [out (ByteArrayOutputStream.)]
      (is (thrown? IllegalArgumentException
                   (write-bencode out {"obj" (Object.)})))
      (is (= "d3:obj" (String. (.toByteArray out)))))))