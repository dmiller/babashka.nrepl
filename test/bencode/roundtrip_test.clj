;; Copyright (c) David Miller. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns bencode.roundtrip-test
  (:require [clojure.test :refer [are deftest is testing]]
            [bencode.core :as bencode :refer [read-bencode
                                              read-netstring
                                              write-bencode
                                              write-netstring]])
  (:import [System.Net Dns IPEndPoint IPAddress]
           [System.Net.Sockets TcpListener TcpClient SocketException]
           [System.IO Stream EndOfStreamException]
		   [clojure.lang PushbackInputStream]))
		   