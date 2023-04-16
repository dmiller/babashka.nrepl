(ns babashka.nrepl.server
  {:author "Michiel Borkent"}
  (:require [babashka.nrepl.impl.server :as server]
            [babashka.nrepl.server.middleware :as middleware]
            [clojure.string :as string]
            )                                         ;;;[sci.core :as sci]
  (:import [System.Net Dns IPEndPoint IPAddress]
           [System.Net.Sockets TcpListener] ))

(set! *warn-on-reflection* true)

(defn stop-server! [{:keys [socket]}]
  (.Stop ^TcpListener socket))

(defn parse-opt [host+port]
  (let [parts (string/split host+port #":")
        [host port] (if (= 1 (count parts))
                         [nil (Int64/Parse ^String (first parts))]                 ;;; Integer.
                         [(first parts)
                          (Int64/Parse  ^String (second parts))])]                 ;;; Integer.
    {:host host
     :port port}))

(defn start-server! [ctx & [{:keys [host port quiet]
                             :or {host "127.0.0.1"
                                  port 1667}
                             :as opts}]]
  (let [ctx (assoc ctx :sessions (atom #{}))
        opts (assoc opts :xform
                    (get opts :xform
                         middleware/default-xform))
		host-entry (Dns/GetHostEntry ^String host)
        ip-address (first (.AddressList host-entry))
		ip-endpoint (IPEndPoint. ^IPAddress ip-address (int port))
        tcp-listener (doto (TcpListener. ip-endpoint) (.Start))  ;; start required here in order to pick up .LocalEndPoint
		local-port (.Port ^IPEndPoint (.LocalEndPoint (.Server tcp-listener)))]
    (when-not quiet
      (println (format "Started nREPL server at %s:%d" (.Address ip-endpoint) local-port)))
    {:socket tcp-listener
     :future (future
               (server/listen ctx tcp-listener opts))}))
