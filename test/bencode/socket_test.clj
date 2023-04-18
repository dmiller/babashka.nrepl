;; Copyright (c) David Miller. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns bencode.socket-test
  (:require [clojure.test :refer [are deftest is testing]]
            [bencode.core :as bencode :refer [read-bencode
                                              read-netstring
                                              write-bencode
                                              write-netstring]])
  (:import [System.Net Dns IPEndPoint IPAddress]
           [System.Net.Sockets TcpListener TcpClient SocketException]
           [System.IO Stream EndOfStreamException]
		   [clojure.lang PushbackInputStream]))

(set! *warn-on-reflection* true)											  
					
(def test-server-port 6666)

(defn process-message [msg os]
  (println "process-message: processing " (class msg) " " msg)
  (cond
    (instance? Int64 msg) (write-bencode os (inc ^int msg))
	(instance? String msg) (write-bencode os (str msg " -- Back at ya!"))
	(map? msg) (write-bencode os (assoc msg :response "You sent a map?"))
    :default (write-bencode os (str msg " -- whatevs"))))
	

(defn session-loop [is os]
  (println "session-loop: reading")
  (when-let [msg (try (read-bencode is)
                      (catch EndOfStreamException _
					      (println "Client closed connection")))]
    (println "session-loop, read " (class msg) " " msg ", off to process")
    (process-message msg os)
	(recur is os)))
				
(defn listen [^TcpListener listener]
  (println "Listening")
  (let [^TcpClient client
          (try 
		    (.AcceptTcpClient listener)
		    (catch SocketException _
               (println "listen: SocketException")
               nil))
	    in (PushbackInputStream. (.GetStream client))
		out (.GetStream client)]
    (println "Connected")
    (future (session-loop in out))
    (recur listener)))
	
				
(defn stop-server [^TcpListener server]
  (.Stop server))					
					
(defn start-server []
  (let [host-entry (Dns/GetHostEntry "127.0.0.1")
        ip-address (first (.AddressList host-entry))
		ip-endpoint (IPEndPoint. ^IPAddress ip-address (int test-server-port))
        tcp-listener (doto (TcpListener. ip-endpoint) (.Start))  ;; start required here in order to pick up .LocalEndPoint
		local-port (.Port ^IPEndPoint (.LocalEndPoint (.Server tcp-listener)))]
    (future (listen tcp-listener))
    tcp-listener))	
	
	
(defmacro server-wrapper [& body]
  `(let [server# (start-server)]
     `@body
     (stop-server server#)))
	 
(defmacro client-wrapper [& body]	
 `(with-open [socket (TcpClient. "127.0.0.1" (int test-server-port))]
     (let [in (.GetStream socket)
           in (clojure.lang.PushbackInputStream. in)
           os (.GetStream socket)]	
    ~@body))			  
    
(deftest write-one
   (server-wrapper 
     (client-wrapper
        (write-bencode os "Here goes nothing!")
        (println (read-bencode in)))))	